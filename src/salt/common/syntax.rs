use core::hash::Hash;
use core::hash::Hasher;
use enum_map::Enum;
use langtools_common::langtools::common::position::BasicPosition;
use langtools_common::langtools::common::symbol::Symbol;
use num_rational::BigRational;
use std::cmp::Ordering;
use std::fmt::Display;
use std::fmt::Debug;
use std::fmt::Formatter;
use std::fmt::Result;

/// Distinguished type for field names.
#[derive(Copy, Clone, Eq, Hash, PartialEq, PartialOrd, Ord)]
pub struct Fieldname<'a>(Symbol<'a>);

impl<'a> Fieldname<'a> {
    /// Create a `Fieldname` for the symbol `sym`.
    pub fn new(sym: Symbol<'a>) -> Fieldname {
        Fieldname(sym)
    }

    /// Get the `Symbol` associated with this `Fieldname`.
    pub fn sym(self) -> Symbol<'a> {
        self.0
    }
}

impl<'a> Display for Fieldname<'a> {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "field {}", self.0)
    }
}

impl<'a> Debug for Fieldname<'a> {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "field {:?}", self.0)
    }
}


/// Associativity.  This is used in syntax directives.
#[derive(Copy, Clone, Enum, Eq, Hash, PartialEq, PartialOrd, Ord)]
pub enum Assoc {
    /// Left-associativity.
    Left,
    /// Right-associativity.
    Right,
    /// Nonassociativity.
    None
}

impl Display for Assoc {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            Assoc::Left => write!(f, "left-associative"),
            Assoc::Right => write!(f, "right-associative"),
            Assoc::None => write!(f, "nonassociative")
        }
    }
}

impl Debug for Assoc {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            Assoc::Left => write!(f, "left-associative"),
            Assoc::Right => write!(f, "right-associative"),
            Assoc::None => write!(f, "nonassociative")
        }
    }
}

/// Fixity.  This is used for syntax directives to denote the fixity
/// behavior of symbols.
#[derive(Copy, Clone, Eq, Hash, PartialEq, PartialOrd, Ord)]
pub enum Fixity {
    /// Prefix symbol.
    Prefix,
    /// Infix symbol.
    Infix {
        /// Associativity of the infix symbol.
        assoc: Assoc
    },
    /// Postfix symbol.
    Postfix
}

impl Display for Fixity {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            Fixity::Prefix => write!(f, "prefix"),
            Fixity::Infix { assoc } => write!(f, "infix {}", assoc),
            Fixity::Postfix => write!(f, "postfix")
        }
    }
}

impl Debug for Fixity {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            Fixity::Prefix => write!(f, "prefix"),
            Fixity::Infix { assoc } => write!(f, "infix {:?}", assoc),
            Fixity::Postfix => write!(f, "postfix")
        }
    }
}

/// Precedence level specifier
#[derive(Clone, Eq, Ord)]
pub enum Level<'a, N> {
    /// Precedence of a specific symbol.
    Name {
        /// The symbol whose precedence to use.
        name: N
    },
    /// Default precedence of prefix symbols.
    Prefix {
        /// The position in source at which this occurs.
        pos: &'a BasicPosition<'a>
    },
    /// Default precedence of infix symbols.
    Infix {
        /// The position in source at which this occurs.
        pos: &'a BasicPosition<'a>
    }
}

impl<'a, N: PartialEq> PartialEq for Level<'a, N> {
    fn eq(&self, other: &Level<'a, N>) -> bool {
        match (self, other) {
            (Level::Name { name: name1 }, Level::Name { name: name2 }) =>
                name1 == name2,
            (Level::Prefix { .. }, Level::Prefix { .. }) => true,
            (Level::Infix { .. }, Level::Infix { .. }) => true,
            _ => false
        }
    }
}

impl<'a, N: PartialOrd> PartialOrd for Level<'a, N> {
    fn partial_cmp(&self, other: &Level<'a, N>) -> Option<Ordering> {
        match (self, other) {
            (Level::Name { name: name1 }, Level::Name { name: name2 }) =>
                name1.partial_cmp(name2),
            (Level::Name { .. }, _) => Some(Ordering::Less),
            (_, Level::Name { .. }) => Some(Ordering::Greater),
            (Level::Prefix { .. }, Level::Prefix { .. }) =>
                Some(Ordering::Equal),
            (Level::Prefix { .. }, _) => Some(Ordering::Less),
            (_, Level::Prefix { .. }) => Some(Ordering::Greater),
            (Level::Infix { .. }, Level::Infix { .. }) => Some(Ordering::Equal)
        }
    }
}


impl<'a, N: Display> Display for Level<'a, N> {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            Level::Name { name } => write!(f, "{}", name),
            Level::Prefix { .. } => write!(f, "prefix"),
            Level::Infix { .. } => write!(f, "infix")
        }
    }
}

impl<'a, N: Debug> Debug for Level<'a, N> {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            Level::Name { name } => write!(f, "{:?}", name),
            Level::Prefix { .. } => write!(f, "prefix"),
            Level::Infix { .. } => write!(f, "infix")
        }
    }
}

impl<'a, N: Hash> Hash for Level<'a, N> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            Level::Name { name } => {
                state.write_u8(0);
                name.hash(state)
            },
            Level::Prefix { .. } => state.write_u8(1),
            Level::Infix { .. } => state.write_u8(2)
        }
    }

}

/// A precedence relation.  This is essentially the symbol and
/// right-hand side of a Wirth-Weber precedence relationship.
#[derive(Clone, Eq, Ord)]
pub struct Prec<'a, N> {
    /// The kind of precedence relation.
    pub ord: Ordering,
    /// The precedence level in the relation.
    pub level: &'a Level<'a, N>,
    /// The position in source at which this occurs.
    pub pos: &'a BasicPosition<'a>
}

impl<'a, N: PartialEq> PartialEq for Prec<'a, N> {
    fn eq(&self, other: &Prec<'a, N>) -> bool {
        self.ord == other.ord && self.level == other.level
    }
}

impl<'a, N: PartialOrd> PartialOrd for Prec<'a, N> {
    fn partial_cmp(&self, other: &Prec<'a, N>) -> Option<Ordering> {
        match self.ord.cmp(&other.ord) {
            Ordering::Equal => self.level.partial_cmp(&other.level),
            out => Some(out)
        }
    }
}

impl<'a, N: Display> Display for Prec<'a, N> {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self.ord {
            Ordering::Less => write!(f, "<< {}", self.level),
            Ordering::Equal => write!(f, "== {}", self.level),
            Ordering::Greater => write!(f, ">> {}", self.level),
        }
    }
}

impl<'a, N: Display> Debug for Prec<'a, N> {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self.ord {
            Ordering::Less => write!(f, "<< {}", self.level),
            Ordering::Equal => write!(f, "== {}", self.level),
            Ordering::Greater => write!(f, ">> {}", self.level),
        }
    }
}

impl<'a, N: Hash> Hash for Prec<'a, N> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.ord.hash(state);
        self.level.hash(state)
    }
}

/// Kinds of elements in the truth environment.
#[derive(Copy, Clone, Enum, Eq, Hash, PartialEq, PartialOrd, Ord)]
pub enum TruthKind {
    /// Theorem, proven within the current truth environment.
    Theorem,
    /// Invariant, assumed by all preconditions, maintained by all
    /// postconditions.
    Invariant,
    Axiom
}

impl Display for TruthKind {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            TruthKind::Theorem => write!(f, "theorem"),
            TruthKind::Invariant => write!(f, "invariant"),
            TruthKind::Axiom => write!(f, "axiom"),
        }
    }
}

impl Debug for TruthKind {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            TruthKind::Theorem => write!(f, "theorem"),
            TruthKind::Invariant => write!(f, "invariant"),
            TruthKind::Axiom => write!(f, "axiom"),
        }
    }
}

#[derive(Copy, Clone, Enum, Eq, Hash, PartialEq, PartialOrd, Ord)]
pub enum ScopeKind {
    Interface,
    Module,
    Class,
    Typeclass,
    Instance
}

impl Display for ScopeKind {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            ScopeKind::Interface => write!(f, "interface"),
            ScopeKind::Module => write!(f, "module"),
            ScopeKind::Class => write!(f, "class"),
            ScopeKind::Typeclass => write!(f, "typeclass"),
            ScopeKind::Instance => write!(f, "instance"),
        }
    }
}

impl Debug for ScopeKind {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            ScopeKind::Interface => write!(f, "interface"),
            ScopeKind::Module => write!(f, "module"),
            ScopeKind::Class => write!(f, "class"),
            ScopeKind::Typeclass => write!(f, "typeclass"),
            ScopeKind::Instance => write!(f, "instance"),
        }
    }
}

#[derive(Copy, Clone, Enum, Eq, Hash, PartialEq, PartialOrd, Ord)]
pub enum ContextKind {
    /// Static definitions.  These are relative only to the global
    /// context (meaning they can only access statically-defined
    /// definitions by name).
    Static,
    /// Local definitions.  These are relative to a function scope,
    /// and can access local definitions by name.
    Local,
    /// Object definitions.  These are relative to an object
    /// definition, but not local to a function scope, and can access
    /// other object definitions by name.
    Object
}

impl Display for ContextKind {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            ContextKind::Static => write!(f, "static"),
            ContextKind::Local => write!(f, "local"),
            ContextKind::Object => write!(f, "object"),
        }
    }
}

impl Debug for ContextKind {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            ContextKind::Static => write!(f, "static"),
            ContextKind::Local => write!(f, "local"),
            ContextKind::Object => write!(f, "object"),
        }
    }
}

/// Kinds of abstractions.  This allows the same abstraction
/// representation to be used for multiple purposes.
#[derive(Copy, Clone, Enum, Eq, Hash, PartialEq, PartialOrd, Ord)]
pub enum AbstractionKind {
    /// A function abstraction.
    Lambda,
    /// Universal quantification.
    Forall,
    /// Existential quantification.
    Exists
}

impl Display for AbstractionKind {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            AbstractionKind::Lambda => write!(f, "lambda"),
            AbstractionKind::Forall => write!(f, "forall"),
            AbstractionKind::Exists => write!(f, "exists"),
        }
    }
}

impl Debug for AbstractionKind {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            AbstractionKind::Lambda => write!(f, "lambda"),
            AbstractionKind::Forall => write!(f, "forall"),
            AbstractionKind::Exists => write!(f, "exists"),
        }
    }
}

#[derive(Copy, Clone, Enum, Eq, Hash, PartialEq, PartialOrd, Ord)]
pub enum Visibility {
    /// Completely hidden visibility.  This is used for
    /// compiler-generated definitions that should never be visible.
    Hidden,
    /// Private visibility.  These are visible only in the defining
    /// scope, and all nested scopes.
    Private,
    /// Protected visibility.  These are visible in the defining
    /// scope, nested scopes, and all inherited scopes.
    Protected,
    /// Public visibility.  These are visible everywhere.
    Public
}

impl Display for Visibility {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            Visibility::Hidden => write!(f, "hidden"),
            Visibility::Private => write!(f, "private"),
            Visibility::Protected => write!(f, "protected"),
            Visibility::Public => write!(f, "public")
        }
    }
}

impl Debug for Visibility {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            Visibility::Hidden => write!(f, "hidden"),
            Visibility::Private => write!(f, "private"),
            Visibility::Protected => write!(f, "protected"),
            Visibility::Public => write!(f, "public")
        }
    }
}

/// Literal values.
#[derive(Clone, Eq, Hash, PartialEq, PartialOrd, Ord)]
pub enum Literal<'a> {
    /// A numeric literal.  This is represented as an unbounded
    /// rational number.
    Num {
        /// The literal value.
        val: &'a BigRational,
    },
    /// A string literal.
    Str {
        /// The literal value.
        val: &'a str
    },
    /// A character literal.
    Char {
        /// The literal value.
        val: char
    }
}

impl<'a> Display for Literal<'a> {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            Literal::Num { val } => write!(f, "{}", val),
            Literal::Str { val } => write!(f, "\"{}\"", val),
            Literal::Char { val } => write!(f, "\'{}\'", val),
        }
    }
}

impl<'a> Debug for Literal<'a> {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            Literal::Num { val } => write!(f, "{}", val),
            Literal::Str { val } => write!(f, "\"{}\"", val),
            Literal::Char { val } => write!(f, "\'{}\'", val),
        }
    }
}
