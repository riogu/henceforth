// The registry of builtin functions: currently its the print and the input_* family. These exist
// because basic I/O needs *something* underneath it that isn't expressible as a normal user
// function (print takes any type; none of the input_* functions have a body to speak of). This
// module is the single source of truth for "what builtins exist". To remove a builtin, delete its
// entry here.

use crate::hfs::{FLOAT_TYPE_ID, INT_TYPE_ID, STRING_TYPE_ID, TypeId};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Builtin {
    Print,
    InputInt,
    InputFloat,
    InputStr,
}

// these are the same primitives every arena pre-allocates (see PRIMITIVE_TYPE_COUNT), so no
// separate type representation is needed here
pub struct BuiltinSpec {
    pub name: &'static str,
    pub builtin: Builtin,
    // print's argument type isn't actually checked, it accepts any type. the placeholder here just
    // gives it a structurally valid signature so it lowers like an ordinary function
    pub params: &'static [TypeId],
    pub returns: &'static [TypeId],
}

pub const BUILTINS: &[BuiltinSpec] = &[
    BuiltinSpec { name: "print", builtin: Builtin::Print, params: &[INT_TYPE_ID], returns: &[] },
    BuiltinSpec { name: "input_int", builtin: Builtin::InputInt, params: &[], returns: &[INT_TYPE_ID] },
    BuiltinSpec { name: "input_float", builtin: Builtin::InputFloat, params: &[], returns: &[FLOAT_TYPE_ID] },
    BuiltinSpec { name: "input_str", builtin: Builtin::InputStr, params: &[], returns: &[STRING_TYPE_ID] },
];

pub fn find_builtin(name: &str) -> Option<&'static BuiltinSpec> { BUILTINS.iter().find(|spec| spec.name == name) }
