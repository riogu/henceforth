use crate::hfs::{InstId, Instruction, IrArena, Literal, TokenKind, UnresolvedAstArena, UnresolvedExprId, ast::*};

pub const PRIMITIVE_TYPE_COUNT: usize = 4;

pub trait Type {
    type IndexType;
    type Arena;
    fn get_repr(&self, arena: &Self::Arena) -> String;
    fn get_ptr_count(&self) -> usize;
    fn new_int(ptr_count: usize) -> Self;
    fn new_string(ptr_count: usize) -> Self;
    fn new_bool(ptr_count: usize) -> Self;
    fn new_float(ptr_count: usize) -> Self;
    fn new_tuple(types: Vec<TypeId>, ptr_count: usize) -> Self;
    fn new_array(hfs_type: TypeId, length: Option<Self::IndexType>, ptr_count: usize) -> Self;
    fn type_id(&self) -> TypeId;
    fn to_token(&self) -> TokenKind;
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum UnresolvedType {
    Int { ptr_count: usize },
    String { ptr_count: usize },
    Bool { ptr_count: usize },
    Float { ptr_count: usize },
    Tuple { type_ids: Vec<TypeId>, ptr_count: usize },
    Array { hfs_type: TypeId, length: Option<UnresolvedExprId>, ptr_count: usize },
}

impl Type for UnresolvedType {
    type IndexType = UnresolvedExprId;
    type Arena = UnresolvedAstArena;

    fn get_repr(&self, arena: &Self::Arena) -> String {
        match self {
            UnresolvedType::Int { ptr_count } => format!("i32{}", "*".repeat(*ptr_count)),
            UnresolvedType::String { ptr_count } => format!("str{}", "*".repeat(*ptr_count)),
            UnresolvedType::Bool { ptr_count } => format!("bool{}", "*".repeat(*ptr_count)),
            UnresolvedType::Float { ptr_count } => format!("f32{}", "*".repeat(*ptr_count)),
            UnresolvedType::Tuple { type_ids, ptr_count } => format!(
                "({}){}",
                type_ids.iter().map(|id| arena.get_type(*id).get_repr(arena)).collect::<Vec<String>>().join(", "),
                "*".repeat(*ptr_count)
            ),
            UnresolvedType::Array { hfs_type, length: _, ptr_count } =>
                format!("[]{}{}", arena.get_type(*hfs_type).get_repr(arena), "*".repeat(*ptr_count)),
        }
    }

    fn get_ptr_count(&self) -> usize {
        match self {
            UnresolvedType::Int { ptr_count } => *ptr_count,
            UnresolvedType::String { ptr_count } => *ptr_count,
            UnresolvedType::Bool { ptr_count } => *ptr_count,
            UnresolvedType::Float { ptr_count } => *ptr_count,
            UnresolvedType::Tuple { type_ids: _, ptr_count } => *ptr_count,
            UnresolvedType::Array { hfs_type: _, length: _, ptr_count } => *ptr_count,
        }
    }

    fn new_int(ptr_count: usize) -> Self { Self::Int { ptr_count } }

    fn new_string(ptr_count: usize) -> Self { Self::String { ptr_count } }

    fn new_bool(ptr_count: usize) -> Self { Self::Bool { ptr_count } }

    fn new_float(ptr_count: usize) -> Self { Self::Float { ptr_count } }

    fn new_tuple(types: Vec<TypeId>, ptr_count: usize) -> Self { Self::Tuple { type_ids: types, ptr_count } }

    fn new_array(hfs_type: TypeId, length: Option<Self::IndexType>, ptr_count: usize) -> Self {
        Self::Array { hfs_type, length, ptr_count }
    }

    fn type_id(&self) -> TypeId {
        match self {
            UnresolvedType::Int { ptr_count: _ } => TypeId(0),
            UnresolvedType::String { ptr_count: _ } => TypeId(3),
            UnresolvedType::Bool { ptr_count: _ } => TypeId(2),
            UnresolvedType::Float { ptr_count: _ } => TypeId(1),
            _ => panic!("[internal error] cannot get type id of non-primitive type"),
        }
    }

    fn to_token(&self) -> TokenKind {
        match self {
            UnresolvedType::Int { .. } => TokenKind::Int,
            UnresolvedType::String { .. } => TokenKind::String,
            UnresolvedType::Bool { .. } => TokenKind::Bool,
            UnresolvedType::Float { .. } => TokenKind::Float,
            UnresolvedType::Tuple { .. } => TokenKind::LeftParen,
            UnresolvedType::Array { .. } => TokenKind::LeftBracket,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ArrayLength {
    Unresolved(UnresolvedExprId),
    Resolved(ExprId),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ElaboratedType {
    Int { ptr_count: usize },
    String { ptr_count: usize },
    Bool { ptr_count: usize },
    Float { ptr_count: usize },
    Tuple { type_ids: Vec<TypeId>, ptr_count: usize },
    Array { hfs_type: TypeId, length: Option<ArrayLength>, ptr_count: usize },
}

impl Type for ElaboratedType {
    type IndexType = ArrayLength;
    type Arena = AstArena;

    fn get_repr(&self, arena: &Self::Arena) -> String {
        match self {
            ElaboratedType::Int { ptr_count } => format!("i32{}", "*".repeat(*ptr_count)),
            ElaboratedType::String { ptr_count } => format!("str{}", "*".repeat(*ptr_count)),
            ElaboratedType::Bool { ptr_count } => format!("bool{}", "*".repeat(*ptr_count)),
            ElaboratedType::Float { ptr_count } => format!("f32{}", "*".repeat(*ptr_count)),
            ElaboratedType::Tuple { type_ids, ptr_count } => format!(
                "({}){}",
                type_ids.iter().map(|id| arena.get_type(*id).get_repr(arena)).collect::<Vec<String>>().join(", "),
                "*".repeat(*ptr_count)
            ),
            ElaboratedType::Array { hfs_type, length, ptr_count } =>
                if matches!(*hfs_type, TypeId(usize::MAX)) {
                    return format!("array");
                } else {
                    return format!(
                        "[{}]{}{}",
                        match length {
                            Some(ArrayLength::Resolved(id)) => match arena.get_expr(*id) {
                                Expression::Literal(Literal::Integer(n)) => n.to_string(),
                                Expression::Literal(_) => panic!("[internal error] typechecking array before its length"),
                                _ => unimplemented!(),
                            },
                            Some(ArrayLength::Unresolved(_)) => format!("array"),
                            None => String::new(),
                        },
                        arena.get_type(*hfs_type).get_repr(arena),
                        "*".repeat(*ptr_count)
                    );
                },
        }
    }

    fn get_ptr_count(&self) -> usize {
        match self {
            ElaboratedType::Int { ptr_count } => *ptr_count,
            ElaboratedType::String { ptr_count } => *ptr_count,
            ElaboratedType::Bool { ptr_count } => *ptr_count,
            ElaboratedType::Float { ptr_count } => *ptr_count,
            ElaboratedType::Tuple { ptr_count, .. } => *ptr_count,
            ElaboratedType::Array { ptr_count, .. } => *ptr_count,
        }
    }

    fn new_int(ptr_count: usize) -> Self { ElaboratedType::Int { ptr_count } }

    fn new_string(ptr_count: usize) -> Self { ElaboratedType::String { ptr_count } }

    fn new_bool(ptr_count: usize) -> Self { ElaboratedType::Bool { ptr_count } }

    fn new_float(ptr_count: usize) -> Self { ElaboratedType::Float { ptr_count } }

    fn new_tuple(types: Vec<TypeId>, ptr_count: usize) -> Self { ElaboratedType::Tuple { type_ids: types, ptr_count } }

    fn new_array(hfs_type: TypeId, length: Option<Self::IndexType>, ptr_count: usize) -> Self {
        ElaboratedType::Array { hfs_type, length, ptr_count }
    }

    fn type_id(&self) -> TypeId {
        match self {
            ElaboratedType::Int { ptr_count: _ } => TypeId(0),
            ElaboratedType::String { ptr_count: _ } => TypeId(3),
            ElaboratedType::Bool { ptr_count: _ } => TypeId(2),
            ElaboratedType::Float { ptr_count: _ } => TypeId(1),
            _ => panic!("[internal error] cannot get type id of non-primitive type"),
        }
    }

    fn to_token(&self) -> TokenKind {
        match self {
            ElaboratedType::Int { .. } => TokenKind::Int,
            ElaboratedType::String { .. } => TokenKind::String,
            ElaboratedType::Bool { .. } => TokenKind::Bool,
            ElaboratedType::Float { .. } => TokenKind::Float,
            ElaboratedType::Tuple { .. } => TokenKind::LeftParen,
            ElaboratedType::Array { .. } => TokenKind::LeftBracket,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum IrType {
    Int { ptr_count: usize },
    String { ptr_count: usize },
    Bool { ptr_count: usize },
    Float { ptr_count: usize },
    Tuple { type_ids: Vec<TypeId>, ptr_count: usize },
    Array { hfs_type: TypeId, length: Option<InstId>, ptr_count: usize },
}

impl Type for IrType {
    type IndexType = InstId;
    type Arena = IrArena;

    fn get_repr(&self, arena: &Self::Arena) -> String {
        match self {
            IrType::Int { ptr_count } => format!("i32{}", "*".repeat(*ptr_count)),
            IrType::String { ptr_count } => format!("str{}", "*".repeat(*ptr_count)),
            IrType::Bool { ptr_count } => format!("bool{}", "*".repeat(*ptr_count)),
            IrType::Float { ptr_count } => format!("f32{}", "*".repeat(*ptr_count)),
            IrType::Tuple { type_ids, ptr_count } => format!(
                "Tuple<{}>{}",
                type_ids.iter().map(|id| arena.get_type(*id).get_repr(arena)).collect::<Vec<String>>().join(", "),
                "*".repeat(*ptr_count)
            ),
            IrType::Array { hfs_type, length, ptr_count } =>
                if matches!(*hfs_type, TypeId(usize::MAX)) {
                    return format!("array");
                } else {
                    return format!(
                        "[{}]{}{}",
                        match length {
                            Some(inst) => match arena.get_inst(*inst) {
                                Instruction::Literal { literal: Literal::Integer(n), .. } => n.to_string(),
                                Instruction::Literal { .. } =>
                                    panic!("[internal error] found literal in length expression of array at codegen"),
                                _ => unimplemented!(),
                            },
                            None => String::new(),
                        },
                        arena.get_type(*hfs_type).get_repr(arena),
                        "*".repeat(*ptr_count)
                    );
                },
        }
    }

    fn get_ptr_count(&self) -> usize {
        match self {
            IrType::Int { ptr_count } => *ptr_count,
            IrType::String { ptr_count } => *ptr_count,
            IrType::Bool { ptr_count } => *ptr_count,
            IrType::Float { ptr_count } => *ptr_count,
            IrType::Tuple { ptr_count, .. } => *ptr_count,
            IrType::Array { ptr_count, .. } => *ptr_count,
        }
    }

    fn new_int(ptr_count: usize) -> Self { IrType::Int { ptr_count } }

    fn new_string(ptr_count: usize) -> Self { IrType::String { ptr_count } }

    fn new_bool(ptr_count: usize) -> Self { IrType::Bool { ptr_count } }

    fn new_float(ptr_count: usize) -> Self { IrType::Float { ptr_count } }

    fn new_tuple(types: Vec<TypeId>, ptr_count: usize) -> Self { IrType::Tuple { type_ids: types, ptr_count } }

    fn new_array(hfs_type: TypeId, length: Option<Self::IndexType>, ptr_count: usize) -> Self {
        IrType::Array { hfs_type, length, ptr_count }
    }

    fn type_id(&self) -> TypeId {
        match self {
            IrType::Int { ptr_count: _ } => TypeId(0),
            IrType::String { ptr_count: _ } => TypeId(3),
            IrType::Bool { ptr_count: _ } => TypeId(2),
            IrType::Float { ptr_count: _ } => TypeId(1),
            _ => panic!("[internal error] cannot get type id of non-primitive type"),
        }
    }

    fn to_token(&self) -> TokenKind {
        match self {
            IrType::Int { .. } => TokenKind::Int,
            IrType::String { .. } => TokenKind::String,
            IrType::Bool { .. } => TokenKind::Bool,
            IrType::Float { .. } => TokenKind::Float,
            IrType::Tuple { .. } => TokenKind::LeftParen,
            IrType::Array { .. } => TokenKind::LeftBracket,
        }
    }
}
