const std = @import("std");
const ClassFile = @import("ClassFile.zig");
const mem = std.mem;
const Allocator = mem.Allocator;
const ArenaAllocator = std.heap.ArenaAllocator;
const assert = std.debug.assert;
const meta = std.meta;

const c = @cImport(@cInclude("zip.h"));

pub const Value = union(enum) {
    void,
    byte: i8,
    short: i16,
    int: i32,
    long: i64,
    char: u16,
    float: f32,
    double: f64,
    boolean: bool,
    return_address: usize,
    reference: ?Reference,

    pub const Reference = union(enum) {
        class,
        array: []Value,
        interface,
    };

    pub const Type = meta.FieldEnum(@This());
};

pub const Frame = struct {
    locals: []Value,
    stack: std.ArrayListUnmanaged(Value),
    constant_pool: ClassFile.ConstantPool,
};

pub const ClassLoader = struct {};

pub const JavaVM = struct {
    pc: usize = undefined,
    stack: std.SegmentedList(Frame, 16) = .{},
    arena: *ArenaAllocator,
    class_registry: std.StringHashMapUnmanaged(ClassFile) = .{},

    const Self = @This();

    pub fn init(allocator: Allocator) !Self {
        const self = Self{
            .arena = try allocator.create(ArenaAllocator),
        };
        errdefer allocator.destroy(self.arena);
        self.arena.* = ArenaAllocator.init(allocator);

        return self;
    }

    pub fn deinit(self: *Self) void {
        const allocator = self.arena.child_allocator;
        self.arena.deinit();
        allocator.destroy(self.arena);
    }

    pub fn invoke(self: *Self, class_name: []const u8, method_name: []const u8, method_args: []const Value) !void {
        const allocator = self.arena.allocator();

        const class_file = try self.getClassFile(class_name);
        const method = class_file.findMethod(method_name).?;
        var method_descriptor = try resolveMethodType(allocator, class_file.constant_pool[method.descriptor_index - 1].utf8);
        defer method_descriptor.deinit(allocator);
        const code = ClassFile.findAttribute(method.attributes, .Code).?.Code;
        try self.stack.append(allocator, Frame{
            .constant_pool = class_file.constant_pool,
            .locals = try allocator.alloc(Value, code.max_locals),
            .stack = try std.ArrayListUnmanaged(Value).initCapacity(allocator, code.max_stack),
        });
        const frame: *Frame = self.stack.at(self.stack.len - 1);

        for (method_args, 0..) |arg, i| {
            frame.locals[i] = arg;
        }

        for (code.code) |instr| {
            defer self.pc += 1;
            switch (instr) {
                .iconst_m1 => frame.stack.appendAssumeCapacity(.{ .int = -1 }),
                .iconst_0 => frame.stack.appendAssumeCapacity(.{ .int = 0 }),
                .iconst_1 => frame.stack.appendAssumeCapacity(.{ .int = 1 }),
                .iconst_2 => frame.stack.appendAssumeCapacity(.{ .int = 2 }),
                .iconst_3 => frame.stack.appendAssumeCapacity(.{ .int = 3 }),
                .iconst_4 => frame.stack.appendAssumeCapacity(.{ .int = 4 }),
                .iconst_5 => frame.stack.appendAssumeCapacity(.{ .int = 5 }),
                .iload_0 => frame.stack.appendAssumeCapacity(frame.locals[0]),
                .iload_1 => frame.stack.appendAssumeCapacity(frame.locals[1]),
                .iload_2 => frame.stack.appendAssumeCapacity(frame.locals[2]),
                .iload_3 => frame.stack.appendAssumeCapacity(frame.locals[3]),
                .iadd => frame.stack.appendAssumeCapacity(.{
                    .int = frame.stack.pop().int + frame.stack.pop().int,
                }),
                .ireturn => {
                    const invoker_frame: *Frame = self.stack.at(self.stack.len - 2);
                    invoker_frame.stack.appendAssumeCapacity(frame.stack.pop());
                    _ = self.stack.pop();
                    return;
                },
                .@"return" => {
                    assert(method_descriptor.return_type == .void);
                    _ = self.stack.pop();
                    return;
                },
                .invokestatic => |index| {
                    switch (frame.constant_pool[index - 1]) {
                        .methodref => |ref| {
                            const class_name_index = frame.constant_pool[ref.class_index - 1].class;
                            const name_and_type = frame.constant_pool[ref.name_and_type_index - 1].name_and_type;
                            const method_type_string = frame.constant_pool[name_and_type.descriptor_index - 1].utf8;
                            var method_desc = try resolveMethodType(allocator, method_type_string);
                            defer method_desc.deinit(allocator);
                            const args = try allocator.alloc(Value, method_desc.params.len);
                            for (method_desc.params, 0..) |param_type, i| {
                                const param = frame.stack.pop();
                                assert(meta.activeTag(param) == meta.activeTag(param_type));
                                args[i] = param;
                            }

                            try self.invoke(
                                frame.constant_pool[class_name_index - 1].utf8,
                                frame.constant_pool[name_and_type.name_index - 1].utf8,
                                args,
                            );
                        },
                        .interface_methodref => @panic("unimplemented"),
                        else => unreachable,
                    }
                },
                .pop => _ = frame.stack.pop(),
                else => std.debug.panic("unimplemented instr: {}\n", .{instr}),
            }
        }

        unreachable;
    }

    fn getClassFile(self: *Self, name: []const u8) !*ClassFile {
        const allocator = self.arena.allocator();
        const gop = try self.class_registry.getOrPut(allocator, name);
        if (gop.found_existing) {
            return gop.value_ptr;
        }

        const ext = ".class";
        var path = try std.ArrayList(u8).initCapacity(allocator, name.len + ext.len);
        defer path.deinit();
        path.appendSliceAssumeCapacity(name);
        path.appendSliceAssumeCapacity(ext);

        var sb: c.zip_stat_t = undefined;
        var i: u64 = 0;
        while (c.zip_stat_index(rt_archive, i, 0, &sb) == 0) : (i += 1) {
            if (mem.eql(u8, mem.span(sb.name), path.items)) {
                const fd = c.zip_fopen_index(rt_archive, i, 0);
                defer _ = c.zip_fclose(fd);
                const buf = try allocator.alloc(u8, sb.size);
                defer allocator.free(buf);
                _ = c.zip_fread(fd, buf.ptr, sb.size);
                gop.value_ptr.* = try ClassFile.init(allocator, buf);
                return gop.value_ptr;
            }
        }

        return error.NoClassDefFoundError;
    }
};

pub const FieldType = union(Value.Type) {
    void,
    byte,
    short,
    int,
    long,
    char,
    float,
    double,
    boolean,
    return_address,
    reference: union(enum) {
        class: []const u8,
        array: *FieldType,
    },
};

fn readFieldType(allocator: Allocator, reader: anytype) !FieldType {
    return switch (try reader.readByte()) {
        'B' => .byte,
        'C' => .char,
        'D' => .double,
        'F' => .float,
        'I' => .int,
        'J' => .long,
        'L' => blk: {
            const start = reader.context.pos;
            while (try reader.readByte() != ';') {}
            const class_name = reader.context.buffer[start .. reader.context.pos - 1];
            break :blk .{ .reference = .{ .class = class_name } };
        },
        'S' => .short,
        'V' => .void,
        'Z' => .boolean,
        '[' => blk: {
            const array = try allocator.create(FieldType);
            errdefer allocator.destroy(array);
            array.* = try readFieldType(allocator, reader);
            break :blk .{ .reference = .{ .array = array } };
        },
        else => unreachable,
    };
}

pub const MethodDescriptor = struct {
    params: []const FieldType,
    return_type: FieldType,

    pub fn deinit(self: *@This(), allocator: Allocator) void {
        allocator.free(self.params);
        self.* = undefined;
    }
};

fn resolveMethodType(allocator: Allocator, method_type_string: []const u8) !MethodDescriptor {
    var fbs = std.io.fixedBufferStream(method_type_string);
    const reader = fbs.reader();
    assert(try reader.readByte() == '(');
    var params = std.ArrayList(FieldType).init(allocator);
    errdefer params.deinit();
    while (fbs.buffer[fbs.pos] != ')') {
        try params.append(try readFieldType(allocator, reader));
    }
    _ = try reader.readByte();

    return MethodDescriptor{
        .params = try params.toOwnedSlice(),
        .return_type = try readFieldType(allocator, reader),
    };
}

var rt_archive: *c.zip_t = undefined;

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var err: c_int = undefined;
    rt_archive = c.zip_open("rt.jar", c.ZIP_RDONLY, &err) orelse {
        var ze: c.zip_error_t = undefined;
        c.zip_error_init_with_code(&ze, err);
        defer c.zip_error_fini(&ze);
        std.log.err("{s}", .{c.zip_error_strerror(&ze)});
        return;
    };
    defer _ = c.zip_close(rt_archive);

    var arg_it = try std.process.argsWithAllocator(allocator);
    defer arg_it.deinit();
    _ = arg_it.skip();

    const file = try std.fs.cwd().openFile("Main.class", .{});
    defer file.close();
    const bytes = try file.readToEndAlloc(allocator, (try file.stat()).size);
    defer allocator.free(bytes);
    var class_file = try ClassFile.init(allocator, bytes);
    defer class_file.deinit();

    var jvm = try JavaVM.init(allocator);
    defer jvm.deinit();

    try jvm.class_registry.put(jvm.arena.allocator(), "Main", class_file);
    try jvm.invoke("Main", "main", &.{});
}
