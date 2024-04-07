const std = @import("std");
const mem = std.mem;
const Allocator = mem.Allocator;
const assert = std.debug.assert;
const meta = std.meta;
const bitflags = @import("bitflags");
const Bitflags = bitflags.Bitflags;
const Instruction = @import("instruction.zig").Instruction;

pub const ConstantPool = []const Constant;
pub const Attributes = []const Attribute;

const Self = @This();

arena: std.heap.ArenaAllocator,

minor_version: u16,
major_version: u16,
constant_pool: ConstantPool,
access_flags: AccFlags,
this_class: u16,
super_class: u16,
interfaces: []const u16,
fields: []const FieldInfo,
methods: []const FieldInfo,
attributes: Attributes,

pub const AccFlags = Bitflags(enum(u16) {
    public = 0x0001,
    final = 0x0010,
    super = 0x0020,
    interface = 0x0200,
    abstract = 0x0400,
    synthetic = 0x1000,
    annotation = 0x2000,
    @"enum" = 0x4000,
});

pub fn init(child_allocator: Allocator, bytes: []const u8) !Self {
    var arena = std.heap.ArenaAllocator.init(child_allocator);
    const allocator = arena.allocator();

    var fbs = std.io.fixedBufferStream(bytes);
    const reader = fbs.reader();

    const magic = try reader.readInt(u32, .big);
    if (magic != 0xCAFEBABE) return error.ClassFormatError;
    const minor_version = try reader.readInt(u16, .big);
    const major_version = try reader.readInt(u16, .big);

    const constant_pool_count = try reader.readInt(u16, .big);
    const constant_pool = try allocator.alloc(Constant, constant_pool_count - 1);

    {
        var i: usize = 0;
        while (i < constant_pool_count - 1) : (i += 1) {
            constant_pool[i] = try readConstant(allocator, reader);
            switch (constant_pool[i]) {
                .long => {
                    i += 1;
                    constant_pool[i] = .{ .long = try reader.readInt(u32, .big) };
                },
                .double => {
                    i += 1;
                    constant_pool[i] = .{ .double = try reader.readInt(u32, .big) };
                },
                else => {},
            }
        }
    }

    const access_flags = try reader.readInt(u16, .big);
    const this_class = try reader.readInt(u16, .big);
    const super_class = try reader.readInt(u16, .big);

    const interfaces_count = try reader.readInt(u16, .big);
    const interfaces = try allocator.alloc(u16, interfaces_count);
    for (0..interfaces_count) |i| {
        interfaces[i] = try reader.readInt(u16, .big);
    }

    const fields_count = try reader.readInt(u16, .big);
    const fields = try allocator.alloc(FieldInfo, fields_count);
    for (0..fields_count) |i| {
        fields[i] = try readFieldInfo(allocator, reader, constant_pool);
    }

    const methods_count = try reader.readInt(u16, .big);
    const methods = try allocator.alloc(FieldInfo, methods_count);
    for (0..methods_count) |i| {
        methods[i] = try readFieldInfo(allocator, reader, constant_pool);
    }

    const attributes_count = try reader.readInt(u16, .big);
    const attributes = try allocator.alloc(Attribute, attributes_count);
    for (0..attributes_count) |i| {
        attributes[i] = try readAttribute(allocator, reader, constant_pool);
    }

    return Self{
        .arena = arena,
        .minor_version = minor_version,
        .major_version = major_version,
        .constant_pool = constant_pool,
        .access_flags = @bitCast(access_flags),
        .this_class = this_class,
        .super_class = super_class,
        .interfaces = interfaces,
        .fields = fields,
        .methods = methods,
        .attributes = attributes,
    };
}

pub fn deinit(self: *Self) void {
    self.arena.deinit();
    self.* = undefined;
}

pub fn findMethod(self: Self, name: []const u8) ?FieldInfo {
    for (self.methods) |method| {
        if (mem.eql(u8, self.constant_pool[method.name_index - 1].utf8, name)) {
            return method;
        }
    }
    return null;
}

// TODO: Use `std.meta.TagPayload` for return type?
// Loose type inference that way?
pub fn findAttribute(attributes: Attributes, comptime tag: Attribute.Type) ?Attribute {
    for (attributes) |attribute| {
        if (meta.activeTag(attribute) == tag) {
            return attribute;
        }
    }
    return null;
}

pub const Constant = union(Type) {
    class: u16,
    fieldref: RefInfo,
    methodref: RefInfo,
    interface_methodref: RefInfo,
    string: u16,
    integer: u32,
    float: u32,
    long: u32,
    double: u32,
    name_and_type: struct {
        name_index: u16,
        descriptor_index: u16,
    },
    utf8: []const u8,
    method_handle: MethodHandle,
    method_type: u16,
    dynamic: DynamicInfo,
    invoke_dynamic: DynamicInfo,
    module: u16,
    package: u16,

    pub const Type = enum(u8) {
        class = 7,
        fieldref = 9,
        methodref = 10,
        interface_methodref = 11,
        string = 8,
        integer = 3,
        float = 4,
        long = 5,
        double = 6,
        name_and_type = 12,
        utf8 = 1,
        method_handle = 15,
        method_type = 16,
        dynamic = 17,
        invoke_dynamic = 18,
        module = 19,
        package = 20,
    };

    pub const RefInfo = struct {
        class_index: u16,
        name_and_type_index: u16,
    };

    pub const MethodHandle = struct {
        reference_kind: Ref,
        reference_index: u16,

        pub const Ref = enum(u8) {
            get_field = 1,
            get_static,
            put_field,
            put_static,
            invoke_virtual,
            invoke_special,
            new_invoke_special,
            invoke_interface,
        };
    };

    pub const DynamicInfo = struct {
        bootstrap_method_attr_index: u16,
        name_and_type_index: u16,
    };
};

pub const FieldInfo = struct {
    access_flags: u16,
    name_index: u16,
    descriptor_index: u16,
    attributes: Attributes,
};

pub const Exception = struct {
    start_pc: u16,
    end_pc: u16,
    handler_pc: u16,
    catch_type: u16,
};

pub const LineNumber = struct {
    start_pc: u16,
    line_number: u16,
};

pub const InnerClass = struct {
    inner_class_info_index: u16,
    outer_class_info_index: u16,
    inner_name_index: u16,
    inner_class_access_flags: u16,
};

pub const Attribute = union(Type) {
    ConstantValue: u16,
    Code: struct {
        max_stack: u16,
        max_locals: u16,
        code: []const Instruction,
        exception_table: []const Exception,
        attributes: Attributes,
    },
    StackMapTable: []const StackMapFrame,
    Exceptions: []const u16,
    InnerClasses: []const InnerClass,
    EnclosingMethod,
    Synthetic,
    Signature: u16,
    SourceFile: u16,
    SourceDebugExtension,
    LineNumberTable: []const LineNumber,
    LocalVariableTable,
    LocalVariableTypeTable,
    Deprecated,
    RuntimeVisibleAnnotations,
    RuntimeInvisibleAnnotations: []const Annotation,
    RuntimeVisibleParameterAnnotations,
    RuntimeInvisibleParameterAnnotations,
    AnnotationDefault,
    BootstrapMethods,
    RuntimeVisibleTypeAnnotations,
    RuntimeInvisibleTypeAnnotations,
    MethodParameters,
    Module,
    ModulePackages,
    ModuleMainClass,
    NestHost,
    NestMembers,
    Record,
    PermittedSubclasses,

    pub const Type = enum {
        ConstantValue,
        Code,
        StackMapTable,
        Exceptions,
        InnerClasses,
        EnclosingMethod,
        Synthetic,
        Signature,
        SourceFile,
        SourceDebugExtension,
        LineNumberTable,
        LocalVariableTable,
        LocalVariableTypeTable,
        Deprecated,
        RuntimeVisibleAnnotations,
        RuntimeInvisibleAnnotations,
        RuntimeVisibleParameterAnnotations,
        RuntimeInvisibleParameterAnnotations,
        AnnotationDefault,
        BootstrapMethods,
        RuntimeVisibleTypeAnnotations,
        RuntimeInvisibleTypeAnnotations,
        MethodParameters,
        Module,
        ModulePackages,
        ModuleMainClass,
        NestHost,
        NestMembers,
        Record,
        PermittedSubclasses,
    };
};

pub const StackMapFrame = union(enum) {
    same_frame,
    same_locals_1_stack_item_frame: [1]VerificationTypeInfo,
    same_locals_1_stack_item_frame_extended: struct {
        offset_delta: u16,
        stack: [1]VerificationTypeInfo,
    },
    chop_frame: u16,
    same_frame_extended: u16,
    append_frame: struct {
        offset_delta: u16,
        locals: []const VerificationTypeInfo,
    },
    full_frame: struct {
        offset_delta: u16,
        locals: []const VerificationTypeInfo,
        stack: []const VerificationTypeInfo,
    },
};

pub const VerificationTypeInfo = union(Item) {
    top,
    integer,
    float,
    double,
    long,
    null,
    uninitialized_this,
    object: u16,
    uninitialized: u16,

    pub const Item = enum(u8) {
        top,
        integer,
        float,
        double,
        long,
        null,
        uninitialized_this,
        object,
        uninitialized,
    };
};

pub const ElementValue = union(enum) {
    byte: u16,
    char: u16,
    double: u16,
    float: u16,
    int: u16,
    long: u16,
    short: u16,
    boolean: u16,
    string: u16,
    enum_class: struct {
        type_name_index: u16,
        const_name_index: u16,
    },
    class: u16,
    annotation_interface: Annotation,
    array_type: []const ElementValue,
};

pub const ElementValuePairs = struct {
    element_name_index: u16,
    value: ElementValue,
};

pub const Annotation = struct {
    type_index: u16,
    element_value_pairs: []const ElementValuePairs,
};

fn readStackMapFrame(allocator: Allocator, reader: anytype) !StackMapFrame {
    const frame_type = try reader.readByte();
    return switch (frame_type) {
        0...63 => .same_frame,
        64...127 => .{ .same_locals_1_stack_item_frame = .{try readVerificationTypeInfo(reader)} },
        247 => .{ .same_locals_1_stack_item_frame_extended = .{
            .offset_delta = try reader.readInt(u16, .big),
            .stack = .{try readVerificationTypeInfo(reader)},
        } },
        248...250 => .{ .chop_frame = try reader.readInt(u16, .big) },
        251 => .{ .same_frame_extended = try reader.readInt(u16, .big) },
        252...254 => blk: {
            const offset_delta = try reader.readInt(u16, .big);
            const number_of_locals = frame_type - 251;
            const locals = try allocator.alloc(VerificationTypeInfo, number_of_locals);
            errdefer allocator.free(locals);
            for (0..number_of_locals) |i| {
                locals[i] = try readVerificationTypeInfo(reader);
            }
            break :blk .{ .append_frame = .{
                .offset_delta = offset_delta,
                .locals = locals,
            } };
        },
        255 => blk: {
            const offset_delta = try reader.readInt(u16, .big);
            const number_of_locals = try reader.readInt(u16, .big);
            const locals = try allocator.alloc(VerificationTypeInfo, number_of_locals);
            errdefer allocator.free(locals);
            for (0..number_of_locals) |i| {
                locals[i] = try readVerificationTypeInfo(reader);
            }
            const number_of_stack_items = try reader.readInt(u16, .big);
            const stack = try allocator.alloc(VerificationTypeInfo, number_of_stack_items);
            errdefer allocator.free(stack);
            for (0..number_of_stack_items) |i| {
                stack[i] = try readVerificationTypeInfo(reader);
            }
            break :blk .{ .full_frame = .{
                .offset_delta = offset_delta,
                .locals = locals,
                .stack = stack,
            } };
        },
        else => unreachable,
    };
}

fn readVerificationTypeInfo(reader: anytype) !VerificationTypeInfo {
    const tag: VerificationTypeInfo.Item = @enumFromInt(try reader.readByte());
    return switch (tag) {
        .top => .top,
        .integer => .integer,
        .float => .float,
        .null => .null,
        .uninitialized_this => .uninitialized_this,
        .object => .{ .object = try reader.readInt(u16, .big) },
        .uninitialized => .{ .uninitialized = try reader.readInt(u16, .big) },
        .long => .long,
        .double => .double,
    };
}

fn readConstant(allocator: Allocator, reader: anytype) !Constant {
    return switch (@as(Constant.Type, @enumFromInt(try reader.readByte()))) {
        .class => .{ .class = try reader.readInt(u16, .big) },
        .fieldref => .{ .fieldref = try readRefInfo(reader) },
        .methodref => .{ .methodref = try readRefInfo(reader) },
        .interface_methodref => .{ .interface_methodref = try readRefInfo(reader) },
        .string => .{ .string = try reader.readInt(u16, .big) },
        .integer => .{ .integer = try reader.readInt(u32, .big) },
        .float => .{ .float = try reader.readInt(u32, .big) },
        .long => .{ .long = try reader.readInt(u32, .big) },
        .double => .{ .double = try reader.readInt(u32, .big) },
        .name_and_type => .{ .name_and_type = .{
            .name_index = try reader.readInt(u16, .big),
            .descriptor_index = try reader.readInt(u16, .big),
        } },
        .utf8 => blk: {
            const length = try reader.readInt(u16, .big);
            const bytes = try allocator.alloc(u8, length);
            try reader.readNoEof(bytes);
            break :blk .{ .utf8 = bytes };
        },
        .method_handle => .{ .method_handle = .{
            .reference_kind = @enumFromInt(try reader.readByte()),
            .reference_index = try reader.readInt(u16, .big),
        } },
        .method_type => .{ .method_type = try reader.readInt(u16, .big) },
        .invoke_dynamic => .{ .invoke_dynamic = .{
            .bootstrap_method_attr_index = try reader.readInt(u16, .big),
            .name_and_type_index = try reader.readInt(u16, .big),
        } },
        else => |tag| std.debug.panic("{}", .{tag}),
    };
}

fn readRefInfo(reader: anytype) !Constant.RefInfo {
    return .{
        .class_index = try reader.readInt(u16, .big),
        .name_and_type_index = try reader.readInt(u16, .big),
    };
}

fn readFieldInfo(allocator: Allocator, reader: anytype, constant_pool: ConstantPool) !FieldInfo {
    const access_flags = try reader.readInt(u16, .big);
    const name_index = try reader.readInt(u16, .big);
    const descriptor_index = try reader.readInt(u16, .big);

    const attributes_count = try reader.readInt(u16, .big);
    const attributes = try allocator.alloc(Attribute, attributes_count);
    for (0..attributes_count) |i| {
        attributes[i] = try readAttribute(allocator, reader, constant_pool);
    }

    return FieldInfo{
        .access_flags = access_flags,
        .name_index = name_index,
        .descriptor_index = descriptor_index,
        .attributes = attributes,
    };
}

fn readAnnotation(allocator: Allocator, reader: anytype) (Allocator.Error || error{EndOfStream})!Annotation {
    const type_index = try reader.readInt(u16, .big);
    const num_element_value_pairs = try reader.readInt(u16, .big);
    const element_value_pairs = try allocator.alloc(ElementValuePairs, num_element_value_pairs);
    for (0..num_element_value_pairs) |i| {
        element_value_pairs[i] = .{
            .element_name_index = try reader.readInt(u16, .big),
            .value = try readElementValue(allocator, reader),
        };
    }

    return Annotation{
        .type_index = type_index,
        .element_value_pairs = element_value_pairs,
    };
}

fn readElementValue(allocator: Allocator, reader: anytype) !ElementValue {
    return switch (try reader.readByte()) {
        'B' => .{ .byte = try reader.readInt(u16, .big) },
        'C' => .{ .char = try reader.readInt(u16, .big) },
        'D' => .{ .double = try reader.readInt(u16, .big) },
        'F' => .{ .float = try reader.readInt(u16, .big) },
        'I' => .{ .int = try reader.readInt(u16, .big) },
        'J' => .{ .long = try reader.readInt(u16, .big) },
        'S' => .{ .short = try reader.readInt(u16, .big) },
        'Z' => .{ .boolean = try reader.readInt(u16, .big) },
        's' => .{ .string = try reader.readInt(u16, .big) },
        'e' => .{ .enum_class = .{
            .type_name_index = try reader.readInt(u16, .big),
            .const_name_index = try reader.readInt(u16, .big),
        } },
        '@' => .{ .annotation_interface = try readAnnotation(allocator, reader) },
        '[' => blk: {
            const num_values = try reader.readInt(u16, .big);
            const values = try allocator.alloc(ElementValue, num_values);
            errdefer allocator.free(values);
            break :blk .{ .array_type = values };
        },
        else => unreachable,
    };
}

fn readAttribute(allocator: Allocator, reader: anytype, constant_pool: ConstantPool) !Attribute {
    const attribute_name_index = try reader.readInt(u16, .big);
    const attribute_length = try reader.readInt(u32, .big);
    const attribute_name = constant_pool[attribute_name_index - 1].utf8;
    const attribute_type = meta.stringToEnum(Attribute.Type, attribute_name).?;
    return switch (attribute_type) {
        .ConstantValue => .{ .ConstantValue = try reader.readInt(u16, .big) },
        .Signature => .{ .Signature = try reader.readInt(u16, .big) },
        .Code => blk: {
            const max_stack = try reader.readInt(u16, .big);
            const max_locals = try reader.readInt(u16, .big);
            const code_length = try reader.readInt(u32, .big);
            var lr = std.io.limitedReader(reader, code_length);
            const code_reader = lr.reader();
            var code = std.ArrayList(Instruction).init(allocator);
            while (true) {
                try code.append(readInstruction(code_reader) catch |err| switch (err) {
                    error.EndOfStream => break,
                    else => return err,
                });
            }

            const exception_table_length = try reader.readInt(u16, .big);
            const exception_table = try allocator.alloc(Exception, exception_table_length);
            for (0..exception_table_length) |i| {
                exception_table[i] = Exception{
                    .start_pc = try reader.readInt(u16, .big),
                    .end_pc = try reader.readInt(u16, .big),
                    .handler_pc = try reader.readInt(u16, .big),
                    .catch_type = try reader.readInt(u16, .big),
                };
            }

            const attributes_count = try reader.readInt(u16, .big);
            const attributes = try allocator.alloc(Attribute, attributes_count);
            for (0..attributes_count) |i| {
                attributes[i] = try readAttribute(allocator, reader, constant_pool);
            }

            break :blk .{ .Code = .{
                .max_stack = max_stack,
                .max_locals = max_locals,
                .code = try code.toOwnedSlice(),
                .exception_table = exception_table,
                .attributes = attributes,
            } };
        },
        .SourceFile => .{ .SourceFile = try reader.readInt(u16, .big) },
        .LineNumberTable => blk: {
            const line_number_table_length = try reader.readInt(u16, .big);
            const line_number_table = try allocator.alloc(LineNumber, line_number_table_length);
            for (0..line_number_table_length) |i| {
                line_number_table[i] = LineNumber{
                    .start_pc = try reader.readInt(u16, .big),
                    .line_number = try reader.readInt(u16, .big),
                };
            }
            break :blk .{ .LineNumberTable = line_number_table };
        },
        .StackMapTable => blk: {
            const number_of_entries = try reader.readInt(u16, .big);
            const entries = try allocator.alloc(StackMapFrame, number_of_entries);
            for (0..number_of_entries) |i| {
                entries[i] = try readStackMapFrame(allocator, reader);
            }
            break :blk .{ .StackMapTable = entries };
        },
        .Exceptions => blk: {
            const number_of_exceptions = try reader.readInt(u16, .big);
            const exception_index_table = try allocator.alloc(u16, number_of_exceptions);
            for (0..number_of_exceptions) |i| {
                exception_index_table[i] = try reader.readInt(u16, .big);
            }
            break :blk .{ .Exceptions = exception_index_table };
        },
        .Deprecated => blk: {
            try reader.skipBytes(attribute_length, .{});
            break :blk .Deprecated;
        },
        .RuntimeVisibleAnnotations => blk: {
            const num_annotations = try reader.readInt(u16, .big);
            const annotations = try allocator.alloc(Annotation, num_annotations);
            for (0..num_annotations) |i| {
                annotations[i] = try readAnnotation(allocator, reader);
            }
            break :blk .{ .RuntimeInvisibleAnnotations = annotations };
        },
        .InnerClasses => blk: {
            const number_of_classes = try reader.readInt(u16, .big);
            const classes = try allocator.alloc(InnerClass, number_of_classes);
            for (0..number_of_classes) |i| {
                classes[i] = .{
                    .inner_class_info_index = try reader.readInt(u16, .big),
                    .outer_class_info_index = try reader.readInt(u16, .big),
                    .inner_name_index = try reader.readInt(u16, .big),
                    .inner_class_access_flags = try reader.readInt(u16, .big),
                };
            }

            break :blk .{ .InnerClasses = classes };
        },
        else => std.debug.panic("unimplemented attribute: {}", .{attribute_type}),
    };
}

fn readInstruction(reader: anytype) !Instruction {
    return switch (@as(Instruction.Opcode, @enumFromInt(try reader.readByte()))) {
        .iload_0 => .iload_0,
        .iload_1 => .iload_1,
        .iload_2 => .iload_2,
        .iload_3 => .iload_3,
        .iconst_m1 => .iconst_m1,
        .iconst_0 => .iconst_0,
        .iconst_1 => .iconst_1,
        .iconst_2 => .iconst_2,
        .iconst_3 => .iconst_3,
        .iconst_4 => .iconst_4,
        .iconst_5 => .iconst_5,
        .invokestatic => .{ .invokestatic = try reader.readInt(u16, .big) },
        .invokespecial => .{ .invokespecial = try reader.readInt(u16, .big) },
        .@"return" => .@"return",
        .pop => .pop,
        .iadd => .iadd,
        .ireturn => .ireturn,
        .ldc => .{ .ldc = try reader.readByte() },
        .nop => .nop,
        .aload_0 => .aload_0,
        .aload_1 => .aload_1,
        .aload_2 => .aload_2,
        .aload_3 => .aload_3,
        .if_acmpeq => .{ .if_acmpeq = try reader.readInt(u16, .big) },
        .if_acmpne => .{ .if_acmpne = try reader.readInt(u16, .big) },
        .goto => .{ .goto = try reader.readInt(u16, .big) },
        .new => .{ .new = try reader.readInt(u16, .big) },
        .dup => .dup,
        .invokevirtual => .{ .invokevirtual = try reader.readInt(u16, .big) },
        .areturn => .areturn,
        .dload_0 => .dload_0,
        .dload_1 => .dload_1,
        .dload_2 => .dload_2,
        .dload_3 => .dload_3,
        .lload_0 => .lload_0,
        .lload_1 => .lload_1,
        .lload_2 => .lload_2,
        .lload_3 => .lload_3,
        .fload_0 => .fload_0,
        .fload_1 => .fload_1,
        .fload_2 => .fload_2,
        .fload_3 => .fload_3,
        .lconst_0 => .lconst_0,
        .lconst_1 => .lconst_1,
        .lcmp => .lcmp,
        .ifeq => .{ .ifeq = try reader.readInt(u16, .big) },
        .ifne => .{ .ifne = try reader.readInt(u16, .big) },
        .iflt => .{ .iflt = try reader.readInt(u16, .big) },
        .ifge => .{ .ifge = try reader.readInt(u16, .big) },
        .ifgt => .{ .ifgt = try reader.readInt(u16, .big) },
        .ifle => .{ .ifle = try reader.readInt(u16, .big) },
        .if_icmpeq => .{ .if_icmpeq = try reader.readInt(u16, .big) },
        .if_icmpne => .{ .if_icmpne = try reader.readInt(u16, .big) },
        .if_icmplt => .{ .if_icmplt = try reader.readInt(u16, .big) },
        .if_icmpge => .{ .if_icmpge = try reader.readInt(u16, .big) },
        .if_icmpgt => .{ .if_icmpgt = try reader.readInt(u16, .big) },
        .if_icmple => .{ .if_icmple = try reader.readInt(u16, .big) },
        .fconst_0 => .fconst_0,
        .fconst_1 => .fconst_1,
        .fconst_2 => .fconst_2,
        .athrow => .athrow,
        .ladd => .ladd,
        .lstore_0 => .lstore_0,
        .lstore_1 => .lstore_1,
        .lstore_2 => .lstore_2,
        .lstore_3 => .lstore_3,
        .istore_0 => .istore_0,
        .istore_1 => .istore_1,
        .istore_2 => .istore_2,
        .istore_3 => .istore_3,
        .fstore_0 => .fstore_0,
        .fstore_1 => .fstore_1,
        .fstore_2 => .fstore_2,
        .fstore_3 => .fstore_3,
        .astore_0 => .astore_0,
        .astore_1 => .astore_1,
        .astore_2 => .astore_2,
        .astore_3 => .astore_3,
        .dstore_0 => .dstore_0,
        .dstore_1 => .dstore_1,
        .dstore_2 => .dstore_2,
        .dstore_3 => .dstore_3,
        .putfield => .{ .putfield = try reader.readInt(u16, .big) },
        .getfield => .{ .getfield = try reader.readInt(u16, .big) },
        .arraylength => .arraylength,
        .isub => .isub,
        .istore => .{ .istore = try reader.readByte() },
        .astore => .{ .astore = try reader.readByte() },
        .iload => .{ .iload = try reader.readByte() },
        .aload => .{ .aload = try reader.readByte() },
        .saload => .saload,
        .iinc => .{ .iinc = .{
            .index = try reader.readByte(),
            .@"const" = try reader.readByteSigned(),
        } },
        .bipush => .{ .bipush = try reader.readByteSigned() },
        .impdep1 => .impdep1,
        .breakpoint => .breakpoint,
        .daload => .daload,
        .castore => .castore,
        .aconst_null => .aconst_null,
        .dconst_0 => .dconst_0,
        .dconst_1 => .dconst_1,
        .baload => .baload,
        .sipush => .{ .sipush = try reader.readInt(u16, .big) },
        .iand => .iand,
        .i2c => .i2c,
        .i2l => .i2l,
        .ishl => .ishl,
        .ior => .ior,
        .ldc_w => .{ .ldc_w = try reader.readInt(u16, .big) },
        .monitorenter => .monitorenter,
        .monitorexit => .monitorexit,
        .caload => .caload,
        .lload => .{ .lload = try reader.readByte() },
        .i2b => .i2b,
        .bastore => .bastore,
        .ifnonnull => .{ .ifnonnull = try reader.readInt(u16, .big) },
        .ifnull => .{ .ifnull = try reader.readInt(u16, .big) },
        .instanceof => .{ .instanceof = try reader.readInt(u16, .big) },
        .checkcast => .{ .checkcast = try reader.readInt(u16, .big) },
        .getstatic => .{ .getstatic = try reader.readInt(u16, .big) },
        .anewarray => .{ .anewarray = try reader.readInt(u16, .big) },
        .fload => .{ .fload = try reader.readByte() },
        .dload => .{ .dload = try reader.readByte() },
        .dstore => .{ .dstore = try reader.readByte() },
        .invokeinterface => .{ .invokeinterface = .{
            .index = try reader.readInt(u16, .big),
            .count = blk: {
                const count = try reader.readByte();
                assert(try reader.readByte() == 0);
                break :blk count;
            },
        } },
        .lsub => .lsub,
        .imul => .imul,
        .dmul => .dmul,
        .newarray => .{ .newarray = @enumFromInt(try reader.readByte()) },
        .dup2 => .dup2,
        .dup2_x1 => .dup2_x1,
        .dup2_x2 => .dup2_x2,
        .iaload => .iaload,
        .aastore => .aastore,
        .aaload => .aaload,
        .putstatic => .{ .putstatic = try reader.readInt(u16, .big) },
        else => |op| std.debug.panic("unimplemented opcode: {x}", .{op}),
    };
}
