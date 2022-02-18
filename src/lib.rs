//!
//! The crate to work with type's blueprints.
//!
//! `Blueprint` is a schema for the type structure.
//! Serialized data of the value should match its type blueprint.
//! Type should be deserializable from any data that matches the blueprint.
//!
//! To associated types with blueprints this crate provides `Blueprinted` trait.
//! `<T as Blueprinted>::BLUEPRINT` is a static reference to the blueprint for the `T`.
//!
//! This crate works without `std`.
//! Blueprints deserialization requires `alloc`.
#![no_std]
#![deny(missing_copy_implementations)]
#![deny(missing_debug_implementations)]
#![deny(missing_docs)]

extern crate self as blueprint;

/// A macro that allows any tokens and yield then unchanged if "alloc" feature is enabled in the crate.
/// Otherwise yields no tokens.
#[macro_export]
#[cfg(feature = "alloc")]
macro_rules! with_alloc {
    ($($tokens:tt)*) => {$($tokens)*};
}

/// A macro that allows any tokens and yield then unchanged if "alloc" feature is enabled in the crate.
/// Otherwise yields no tokens.
#[macro_export]
#[cfg(not(feature = "alloc"))]
macro_rules! with_alloc {
    ($($tokens:tt)*) => {};
}

#[cfg(feature = "alloc")]
extern crate alloc;

#[cfg(feature = "alloc")]
use alloc::{boxed::Box, collections::VecDeque, string::String, vec::Vec};

use core::{fmt, ops::Deref};

#[cfg(feature = "derive")]
pub use blueprint_proc::Blueprinted;

/// Wrapper type to generalize reference and box.
#[derive(Debug, PartialEq, PartialOrd)]
#[cfg_attr(not(feature = "alloc"), repr(transparent))]
pub enum RefBox<'a, T: ?Sized> {
    /// Reference variant.
    Ref(&'a T),

    /// Box variant.
    ///
    /// Available only with `alloc` feature (enabled by default).
    #[cfg_attr(docsrs, doc(cfg(feature = "alloc")))]
    #[cfg(feature = "alloc")]
    Box(Box<T>),
}

impl<T: ?Sized> RefBox<'_, T> {
    /// Returns reference to wrapped value.
    #[inline]
    pub const fn get(&self) -> &T {
        match self {
            RefBox::Ref(r) => r,
            #[cfg(feature = "alloc")]
            RefBox::Box(b) => b,
        }
    }

    /// Converts `&'a RefBox<'_, T>` to `RefBox<'a, T>` without cloning.
    #[inline]
    pub const fn make_ref(&self) -> RefBox<'_, T> {
        match *self {
            RefBox::Ref(r) => RefBox::Ref(r),
            #[cfg(feature = "alloc")]
            RefBox::Box(ref b) => RefBox::Ref(b),
        }
    }
}

// impl<T: ?Sized> fmt::Display for RefBox<'_, T>
// where
//     T: fmt::Display,
// {
//     fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
//         fmt::Display::fmt(self.get(), f)
//     }
// }

impl<T: ?Sized> RefBox<'_, T>
where
    T: Clone,
{
    /// Returns owned value. Clones in needed
    pub fn into_owned(self) -> T {
        match self {
            RefBox::Ref(r) => r.clone(),

            #[cfg(feature = "alloc")]
            RefBox::Box(b) => *b,
        }
    }
}

#[cfg(feature = "alloc")]
impl<T: ?Sized> RefBox<'_, T>
where
    T: Clone,
{
    /// Returns boxed value. Clones and boxes in needed.
    /// Cheaper than `Box::new(self.into_owned())` if self is `Box` variant.
    pub fn into_boxed(self) -> Box<T> {
        match self {
            RefBox::Ref(r) => Box::new(r.clone()),
            RefBox::Box(b) => b,
        }
    }
}

impl<'a, T: ?Sized> From<&'a T> for RefBox<'a, T> {
    fn from(r: &'a T) -> Self {
        RefBox::Ref(r)
    }
}

#[cfg(feature = "alloc")]
impl<T: ?Sized> From<Box<T>> for RefBox<'_, T> {
    fn from(b: Box<T>) -> Self {
        RefBox::Box(b)
    }
}

#[cfg(not(feature = "alloc"))]
impl<T: ?Sized> Clone for RefBox<'_, T> {
    #[inline]
    fn clone(&self) -> Self {
        match self {
            RefBox::Ref(r) => RefBox::Ref(*r),
        }
    }
}

#[cfg(feature = "alloc")]
impl<T: ?Sized> Clone for RefBox<'_, T>
where
    Box<T>: Clone,
{
    #[inline]
    fn clone(&self) -> Self {
        match self {
            RefBox::Ref(r) => RefBox::Ref(*r),
            RefBox::Box(b) => RefBox::Box(b.clone()),
        }
    }
}

impl<T: ?Sized> Deref for RefBox<'_, T> {
    type Target = T;

    #[inline]
    fn deref(&self) -> &T {
        self.get()
    }
}

impl<T: ?Sized> AsRef<T> for RefBox<'_, T> {
    #[inline]
    fn as_ref(&self) -> &T {
        self.get()
    }
}

#[cfg(feature = "serde")]
impl<T: ?Sized> serde::ser::Serialize for RefBox<'_, T>
where
    T: serde::ser::Serialize,
{
    #[inline]
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::ser::Serializer,
    {
        match self {
            RefBox::Ref(r) => T::serialize(r, serializer),
            #[cfg(feature = "alloc")]
            RefBox::Box(b) => T::serialize(b, serializer),
        }
    }
}

#[cfg(all(feature = "alloc", feature = "serde"))]
impl<'de, T: ?Sized> serde::de::Deserialize<'de> for RefBox<'_, T>
where
    Box<T>: serde::de::Deserialize<'de>,
{
    #[inline]
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::de::Deserializer<'de>,
    {
        let b = <Box<T> as serde::de::Deserialize<'de>>::deserialize(deserializer)?;
        Ok(RefBox::Box(b))
    }
}

/// Provides constant reference to blueprint for the type.
pub trait Blueprinted {
    /// Reference to blueprint for the type.
    const BLUEPRINT: &'static Blueprint<'static>;
}

impl<T> Blueprinted for RefBox<'_, T>
where
    T: Blueprinted + ?Sized,
{
    const BLUEPRINT: &'static Blueprint<'static> = T::BLUEPRINT;
}

/// Blueprint for a type.
/// Provides runtime type information to safely construct/deconstruct
/// values of the type.
#[derive(Clone, Debug, PartialEq, PartialOrd)]
#[cfg_attr(feature = "serde", derive(serde::Serialize))]
#[cfg_attr(all(feature = "alloc", feature = "serde"), derive(serde::Deserialize))]
pub struct Blueprint<'a> {
    /// Name of the blueprint.
    pub name: RefBox<'a, str>,

    /// Blueprint schema for the type
    pub kind: BlueprintKind<'a>,
}

impl fmt::Display for Blueprint<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}: {}", self.name.get(), self.kind)
    }
}

/// Kind of blueprint for integers.
/// Includes optional limits on values.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct IntegerBlueprint {
    /// Minimal value for the number.
    #[cfg_attr(
        feature = "serde",
        serde(default, skip_serializing_if = "Option::is_none")
    )]
    pub min: Option<i128>,

    /// Maximal value for the number.
    #[cfg_attr(
        feature = "serde",
        serde(default, skip_serializing_if = "Option::is_none")
    )]
    pub max: Option<i128>,
}

impl fmt::Display for IntegerBlueprint {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match (self.min, self.max) {
            (None, None) => f.write_str("Integer"),
            (Some(min), None) => write!(f, "Integer <{}..>", min),
            (None, Some(max)) => write!(f, "Integer <..={}>", max),
            (Some(min), Some(max)) if min == max => {
                write!(f, "Integer < ={} >", min)
            }
            (Some(min), Some(max)) => {
                write!(f, "Integer <{}..={}>", min, max)
            }
        }
    }
}

/// Kind of blueprint for reals.
/// Includes limits on values.
/// Note that `NaN` values of floating point types are considered invalid.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct RealBlueprint {
    /// Minimal value for the number.
    #[cfg_attr(
        feature = "serde",
        serde(
            default = "default_min_real",
            skip_serializing_if = "is_default_min_real"
        )
    )]
    pub min: f64,

    /// Maximal value for the number.
    #[cfg_attr(
        feature = "serde",
        serde(
            default = "default_max_real",
            skip_serializing_if = "is_default_max_real"
        )
    )]
    pub max: f64,
}

impl fmt::Display for RealBlueprint {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match (
            is_default_min_real(&self.min),
            is_default_max_real(&self.max),
        ) {
            (true, true) => f.write_str("Real"),
            (false, true) => write!(f, "Real <{}..>", self.min),
            (true, false) => write!(f, "Real <..={}>", self.max),
            (false, false) if self.min == self.max => {
                write!(f, "Real < = {} >", self.min)
            }
            (false, false) => {
                write!(f, "Real <{}..={}>", self.min, self.max)
            }
        }
    }
}

/// Kind of blueprint for strings.
/// Includes limits on length.
/// May have optimal regex.
#[derive(Clone, Debug, PartialEq, PartialOrd)]
#[cfg_attr(feature = "serde", derive(serde::Serialize))]
#[cfg_attr(all(feature = "alloc", feature = "serde"), derive(serde::Deserialize))]
pub struct StringBlueprint<'a> {
    /// Regex for the string value.
    #[cfg_attr(
        feature = "serde",
        serde(default, skip_serializing_if = "Option::is_none")
    )]
    pub regex: Option<RefBox<'a, str>>,

    /// Minimal length for the string value.
    #[cfg_attr(
        feature = "serde",
        serde(
            default = "default_min_size",
            skip_serializing_if = "is_default_min_size"
        )
    )]
    pub min_len: usize,

    /// Maximal length for the string value.
    #[cfg_attr(
        feature = "serde",
        serde(
            default = "default_max_size",
            skip_serializing_if = "is_default_max_size"
        )
    )]
    pub max_len: usize,
}

impl fmt::Display for StringBlueprint<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.regex.as_deref() {
            None => match (self.min_len, self.max_len) {
                (0, usize::MAX) => f.write_str("String"),
                (min_len, usize::MAX) => write!(f, "String <{}..>", min_len),
                (0, max_len) => write!(f, "String <..={}>", max_len),
                (min_len, max_len) if min_len == max_len => {
                    write!(f, "String < ={} >", min_len)
                }
                (min_len, max_len) => {
                    write!(f, "String <{}..={}>", min_len, max_len)
                }
            },
            Some(regex) => match (self.min_len, self.max_len) {
                (0, usize::MAX) => write!(f, "String <r'{}'>", regex),
                (min_len, usize::MAX) => {
                    write!(f, "String <r'{}', {}..>", regex, min_len)
                }
                (0, max_len) => write!(f, "String <r'{}', ..={}>", regex, max_len),
                (min_len, max_len) if min_len == max_len => {
                    write!(f, "String <r'{}', ={} >", regex, min_len)
                }
                (min_len, max_len) => {
                    write!(f, "String <r'{}', {}..={}>", regex, min_len, max_len)
                }
            },
        }
    }
}

/// Kind of blueprint for sequences.
/// Includes limits on length.
#[derive(Clone, Debug, PartialEq, PartialOrd)]
#[cfg_attr(feature = "serde", derive(serde::Serialize))]
#[cfg_attr(all(feature = "alloc", feature = "serde"), derive(serde::Deserialize))]
pub struct SequenceBlueprint<'a> {
    /// Blueprint of the sequence elements.
    /// Typically derived from element's type.
    /// May include additional restrictions.
    pub element: RefBox<'a, Blueprint<'a>>,

    /// Minimal length for the sequence.
    #[cfg_attr(
        feature = "serde",
        serde(
            default = "default_min_size",
            skip_serializing_if = "is_default_min_size"
        )
    )]
    pub min_len: usize,

    /// Maximal length for the sequence.
    #[cfg_attr(
        feature = "serde",
        serde(
            default = "default_max_size",
            skip_serializing_if = "is_default_max_size"
        )
    )]
    pub max_len: usize,
}

impl fmt::Display for SequenceBlueprint<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match (self.min_len, self.max_len) {
            (0, usize::MAX) => write!(f, "[{}]", self.element.get()),
            (min_len, usize::MAX) => write!(f, "[{}; {}..]", self.element.get(), min_len),
            (0, max_len) => write!(f, "[{}; ..={}]", self.element.get(), max_len),
            (min_len, max_len) if min_len == max_len => {
                write!(f, "[{}; {}]", self.element.get(), min_len)
            }
            (min_len, max_len) => {
                write!(f, "[{}; {}..={}]", self.element.get(), min_len, max_len)
            }
        }
    }
}

/// Kind of blueprint for mappings.
/// Includes limits on entires number.
#[derive(Clone, Debug, PartialEq, PartialOrd)]
#[cfg_attr(feature = "serde", derive(serde::Serialize))]
#[cfg_attr(all(feature = "alloc", feature = "serde"), derive(serde::Deserialize))]
pub struct MappingBlueprint<'a> {
    /// Blueprint for keys.
    pub key: RefBox<'a, Blueprint<'a>>,

    /// Blueprint for values.
    pub value: RefBox<'a, Blueprint<'a>>,

    /// Minimal number of entries.
    #[cfg_attr(
        feature = "serde",
        serde(
            default = "default_min_size",
            skip_serializing_if = "is_default_min_size"
        )
    )]
    pub min_size: usize,

    /// Maximal number of entries.
    #[cfg_attr(
        feature = "serde",
        serde(
            default = "default_max_size",
            skip_serializing_if = "is_default_max_size"
        )
    )]
    pub max_size: usize,
}

impl fmt::Display for MappingBlueprint<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match (self.min_size, self.max_size) {
            (0, usize::MAX) => write!(f, "[{} => {}]", self.key.get(), self.value.get()),
            (min_size, usize::MAX) => write!(
                f,
                "[{} => {}; {}..]",
                self.key.get(),
                self.value.get(),
                min_size
            ),
            (0, max_size) => write!(
                f,
                "[{} => {}; ..={}]",
                self.key.get(),
                self.value.get(),
                max_size
            ),
            (min_size, max_size) if min_size == max_size => {
                write!(
                    f,
                    "[{} => {}; {}]",
                    self.key.get(),
                    self.value.get(),
                    min_size,
                )
            }
            (min_size, max_size) => {
                write!(
                    f,
                    "[{} => {}; {}..={}]",
                    self.key.get(),
                    self.value.get(),
                    min_size,
                    max_size
                )
            }
        }
    }
}

/// Kind of blueprint for tuples.
#[derive(Clone, Debug, PartialEq, PartialOrd)]
#[cfg_attr(feature = "serde", derive(serde::Serialize))]
#[cfg_attr(all(feature = "alloc", feature = "serde"), derive(serde::Deserialize))]
pub struct TupleBlueprint<'a> {
    /// Blueprints for elements of the tuple.
    #[cfg_attr(
        feature = "serde",
        serde(
            default = "default_refbox_slice",
            skip_serializing_if = "<[_]>::is_empty"
        )
    )]
    pub elements: RefBox<'a, [RefBox<'a, Blueprint<'a>>]>,
}

impl fmt::Display for TupleBlueprint<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut elements = self.elements.get().iter();
        match elements.next() {
            None => f.write_str("()"),
            Some(first) => {
                write!(f, "({}", first.get())?;
                for element in elements {
                    write!(f, ", {}", element.get())?;
                }
                f.write_str(")")
            }
        }
    }
}

/// Kind of blueprint for structures.
#[derive(Clone, Debug, PartialEq, PartialOrd)]
#[cfg_attr(feature = "serde", derive(serde::Serialize))]
#[cfg_attr(all(feature = "alloc", feature = "serde"), derive(serde::Deserialize))]
pub struct StructBlueprint<'a> {
    /// Names and blueprints of the fields.
    #[cfg_attr(
        feature = "serde",
        serde(
            default = "default_refbox_slice",
            skip_serializing_if = "<[_]>::is_empty"
        )
    )]
    pub fields: RefBox<'a, [FieldBlueprint<'a>]>,
}

impl fmt::Display for StructBlueprint<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut fields = self.fields.get().iter();
        match fields.next() {
            None => f.write_str("{}"),
            Some(first) => {
                write!(f, "{{ {}", first)?;
                for field in fields {
                    write!(f, ", {}", field)?;
                }
                f.write_str(" }")
            }
        }
    }
}

/// Kind of blueprint for enums.
#[derive(Clone, Debug, PartialEq, PartialOrd)]
#[cfg_attr(feature = "serde", derive(serde::Serialize))]
#[cfg_attr(all(feature = "alloc", feature = "serde"), derive(serde::Deserialize))]
pub struct EnumBlueprint<'a> {
    /// Names and blueprints of the variants.
    #[cfg_attr(
        feature = "serde",
        serde(
            default = "default_refbox_slice",
            skip_serializing_if = "<[_]>::is_empty"
        )
    )]
    pub variants: RefBox<'a, [VariantBlueprint<'a>]>,
}

impl fmt::Display for EnumBlueprint<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut variants = self.variants.get().iter();
        match variants.next() {
            None => f.write_str("{{}}"),
            Some(first) => {
                write!(f, "{{ {}", first)?;
                for variant in variants {
                    write!(f, " | {}", variant)?;
                }
                f.write_str("}")
            }
        }
    }
}

/// Kinds of blueprints.
#[derive(Clone, Debug, PartialEq, PartialOrd)]
#[cfg_attr(feature = "serde", derive(serde::Serialize))]
#[cfg_attr(all(feature = "alloc", feature = "serde"), derive(serde::Deserialize))]
pub enum BlueprintKind<'a> {
    /// Blueprint of the unit type.
    Unit,

    /// Blueprint of the integer numeric type.
    Integer(IntegerBlueprint),

    /// Blueprint of the real numeric type.
    Real(RealBlueprint),

    /// Blueprint of the string type.
    String(StringBlueprint<'a>),

    /// Blueprint for optional value.
    Option(RefBox<'a, BlueprintKind<'a>>),

    /// Sequence blueprint.
    /// Any kind of sequences fall to this kind.
    /// Arrays, slices, `Vec` etc.
    Sequence(SequenceBlueprint<'a>),

    /// Mapping blueprint.
    /// Any kind of mapping fall to this kind.
    /// `HashMap`, `BTreeMap` etc.
    Mapping(MappingBlueprint<'a>),

    /// Blueprint of the tuple struct type.
    Tuple(TupleBlueprint<'a>),

    /// Blueprint of the struct type.
    Struct(StructBlueprint<'a>),

    /// Blueprint of the enum type.
    Enum(EnumBlueprint<'a>),
}

impl fmt::Display for BlueprintKind<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            BlueprintKind::Unit => Ok(()),
            BlueprintKind::Integer(kind) => fmt::Display::fmt(kind, f),
            BlueprintKind::Real(kind) => fmt::Display::fmt(kind, f),
            BlueprintKind::String(kind) => fmt::Display::fmt(kind, f),
            BlueprintKind::Sequence(kind) => fmt::Display::fmt(kind, f),
            BlueprintKind::Mapping(kind) => fmt::Display::fmt(kind, f),
            BlueprintKind::Tuple(kind) => fmt::Display::fmt(kind, f),
            BlueprintKind::Struct(kind) => fmt::Display::fmt(kind, f),
            BlueprintKind::Enum(kind) => fmt::Display::fmt(kind, f),
            BlueprintKind::Option(kind) => write!(f, "Option ({})", kind.get()),
        }
    }
}

impl BlueprintKind<'_> {
    /// Checks if blueprint is of integer kind.
    pub fn is_integer(&self) -> bool {
        core::matches!(self, BlueprintKind::Integer { .. })
    }

    /// Checks if blueprint is of real kind.
    pub fn is_real(&self) -> bool {
        core::matches!(self, BlueprintKind::Integer { .. })
    }

    /// Checks if blueprint is of numeric kind.
    pub fn is_numeric(&self) -> bool {
        core::matches!(
            self,
            BlueprintKind::Integer { .. } | BlueprintKind::Real { .. }
        )
    }

    /// Checks if blueprint is of string kind.
    pub fn is_string(&self) -> bool {
        core::matches!(self, BlueprintKind::String { .. })
    }
}

/// Blueprint for named field.
#[derive(Clone, Debug, PartialEq, PartialOrd)]
#[cfg_attr(feature = "serde", derive(serde::Serialize))]
#[cfg_attr(all(feature = "alloc", feature = "serde"), derive(serde::Deserialize))]
pub struct FieldBlueprint<'a> {
    /// Name of the field.
    pub name: RefBox<'a, str>,

    /// Blueprint of the field.
    /// Typically derived from field's type.
    /// May include additional restrictions.
    pub blueprint: RefBox<'a, Blueprint<'a>>,
}

impl fmt::Display for FieldBlueprint<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}: {}", self.name.get(), self.blueprint.get())
    }
}

/// Blueprint for enum variant.
#[derive(Clone, Debug, PartialEq, PartialOrd)]
#[cfg_attr(feature = "serde", derive(serde::Serialize))]
#[cfg_attr(all(feature = "alloc", feature = "serde"), derive(serde::Deserialize))]
pub struct VariantBlueprint<'a> {
    /// Variant name.
    pub name: RefBox<'a, str>,

    /// Blueprint of the variant.
    pub kind: VariantBlueprintKind<'a>,
}

impl fmt::Display for VariantBlueprint<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}{}", self.name.get(), self.kind)
    }
}

/// Blueprint for named field.
#[derive(Clone, Debug, PartialEq, PartialOrd)]
#[cfg_attr(feature = "serde", derive(serde::Serialize))]
#[cfg_attr(all(feature = "alloc", feature = "serde"), derive(serde::Deserialize))]
pub enum VariantBlueprintKind<'a> {
    /// Blueprint of the unit enum variant.
    Unit,

    /// Blueprint of the tuple enum variant.
    Tuple(TupleBlueprint<'a>),

    /// Blueprint of the struct enum variant.
    Struct(StructBlueprint<'a>),
}

impl fmt::Display for VariantBlueprintKind<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            VariantBlueprintKind::Unit => Ok(()),
            VariantBlueprintKind::Tuple(kind) => fmt::Display::fmt(kind, f),
            VariantBlueprintKind::Struct(kind) => fmt::Display::fmt(kind, f),
        }
    }
}

macro_rules! for_sized {
    ($any:ident $(<$($t:ident)+>)? { kind: $kind:expr }) => {
        impl $(<$($t: Blueprinted)+>)? Blueprinted for $any $(<$($t)+>)? {
            const BLUEPRINT: &'static Blueprint<'static> = &Blueprint {
                name: RefBox::Ref(stringify!($any)),
                kind: $kind,
            };
        }
    };
}

macro_rules! for_integers {
    ($integer:ident) => {
        for_sized!($integer {
            kind: BlueprintKind::Integer(IntegerBlueprint {
                min: Some($integer::MIN as _),
                max: Some($integer::MAX as _),
            })
        });
    };
}

macro_rules! for_real {
    ($real:ident) => {
        for_sized!($real {
            kind: BlueprintKind::Real(RealBlueprint {
                min: f64::NEG_INFINITY,
                max: f64::INFINITY,
            })
        });
    };
}

macro_rules! for_string {
    ($string:ident) => {
        for_sized!($string {
            kind: BlueprintKind::String(StringBlueprint {
                regex: None,
                min_len: 0,
                max_len: usize::MAX,
            })
        });
    };
}

#[cfg(feature = "alloc")]
macro_rules! for_sequence {
    ($sequence:ident<T>) => {
        for_sized!($sequence<T> {
            kind: BlueprintKind::Sequence(SequenceBlueprint {
                element: RefBox::Ref(<T as Blueprinted>::BLUEPRINT),
                min_len: 0,
                max_len: usize::MAX,
            })
        });
    };
}

for_integers!(u8);
for_integers!(u16);
for_integers!(u32);
for_integers!(u64);

for_integers!(i8);
for_integers!(i16);
for_integers!(i32);
for_integers!(i64);
for_integers!(i128);

#[cfg(any(
    target_pointer_width = "8",
    target_pointer_width = "16",
    target_pointer_width = "32",
    target_pointer_width = "64"
))]
for_integers!(usize);

#[cfg(any(
    target_pointer_width = "8",
    target_pointer_width = "16",
    target_pointer_width = "32",
    target_pointer_width = "64",
    target_pointer_width = "128"
))]
for_integers!(isize);

for_real!(f32);
for_real!(f64);

for_string!(str);

impl<T> Blueprinted for [T]
where
    T: Blueprinted,
{
    const BLUEPRINT: &'static Blueprint<'static> = &Blueprint {
        name: RefBox::Ref("slice"),
        kind: BlueprintKind::Sequence(SequenceBlueprint {
            element: RefBox::Ref(<T as Blueprinted>::BLUEPRINT),
            min_len: 0,
            max_len: usize::MAX,
        }),
    };
}

impl<const N: usize, T> Blueprinted for [T; N]
where
    T: Blueprinted,
{
    const BLUEPRINT: &'static Blueprint<'static> = &Blueprint {
        name: RefBox::Ref("array"),
        kind: BlueprintKind::Sequence(SequenceBlueprint {
            element: RefBox::Ref(<T as Blueprinted>::BLUEPRINT),
            min_len: N,
            max_len: N,
        }),
    };
}

macro_rules! for_tuple {
    () => {
        for_tuple!(for A B C D E F G H I J K L M N O P);
    };

    (for) => {
        for_tuple!(impl);
    };

    (for $head:ident $($tail:ident)*) => {
        for_tuple!(for $($tail)*);
        for_tuple!(impl $head $($tail)*);
    };

    (impl) => {
        impl Blueprinted for () {
            const BLUEPRINT: &'static Blueprint<'static> = &Blueprint {
                name: RefBox::Ref("unit"),
                kind: BlueprintKind::Unit,
            };
        }

    };

    (impl $($a:ident)+) => {
        impl<$($a, )+> Blueprinted for ($($a,)+)
        where $($a: Blueprinted,)+
        {
            const BLUEPRINT: &'static Blueprint<'static> = &Blueprint {
                name: RefBox::Ref("tuple"),
                kind: BlueprintKind::Tuple( TupleBlueprint { elements: RefBox::Ref(&[ $( RefBox::Ref($a::BLUEPRINT), )+ ]) } ),
            };
        }
    };
}

for_tuple!();

impl<T> Blueprinted for Option<T>
where
    T: Blueprinted,
{
    const BLUEPRINT: &'static Blueprint<'static> = &Blueprint {
        name: T::BLUEPRINT.name.make_ref(),
        kind: BlueprintKind::Option(RefBox::Ref(&T::BLUEPRINT.kind)),
    };
}

#[cfg(feature = "alloc")]
for_string!(String);

#[cfg(feature = "alloc")]
for_sequence!(Vec<T>);

#[cfg(feature = "alloc")]
for_sequence!(VecDeque<T>);

#[cfg(feature = "alloc")]
impl<T> Blueprinted for Box<T>
where
    T: Blueprinted,
{
    const BLUEPRINT: &'static Blueprint<'static> = T::BLUEPRINT;
}

#[inline]
fn is_default_min_real(min: &f64) -> bool {
    *min == default_min_real()
}

#[inline]
fn is_default_max_real(max: &f64) -> bool {
    *max == default_max_real()
}

#[cfg(feature = "serde")]
#[inline]
fn is_default_min_size(min: &usize) -> bool {
    *min == default_min_size()
}

#[cfg(feature = "serde")]
#[inline]
fn is_default_max_size(max: &usize) -> bool {
    *max == default_max_size()
}

#[inline]
fn default_min_real() -> f64 {
    f64::NEG_INFINITY
}

#[inline]
fn default_max_real() -> f64 {
    f64::INFINITY
}

#[cfg(feature = "serde")]
#[inline]
fn default_min_size() -> usize {
    0
}

#[cfg(feature = "serde")]
#[inline]
fn default_max_size() -> usize {
    usize::MAX
}

#[cfg(all(feature = "alloc", feature = "serde"))]
#[inline]
fn default_refbox_slice<'a, T>() -> RefBox<'a, [T]> {
    RefBox::Ref(&[])
}
