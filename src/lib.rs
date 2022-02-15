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
//! `Blueprinted` trait can be derived to procedurally for most types.
//! `derive(Blueprinted)` can use some serde attributes to match reflection and serialization.
//! For example `serde(rename)` would rename fields or variants in reflection as well.
//!
//! If a filed of the type does not implement `Blueprinted`, consider using attribute
//! `#[blueprint(as TypeName)]` where `TypeName` is a path to the type that implements `Blueprinted`
//! Blueprint of the field would be `TypeName::BLUEPRINT`.
//!
//! This crate works without `std`.
//! Blueprints deserialization requires `alloc`.
#![no_std]
extern crate self as blueprint;

#[macro_export]
#[cfg(feature = "alloc")]
macro_rules! with_alloc {
    ($($tokens:tt)*) => {$($tokens)*};
}

#[macro_export]
#[cfg(not(feature = "alloc"))]
macro_rules! with_alloc {
    ($($tokens:tt)*) => {};
}

#[macro_export]
#[cfg(feature = "regex")]
macro_rules! with_regex {
    ($($tokens:tt)*) => {$($tokens)*};
}

#[macro_export]
#[cfg(not(feature = "regex"))]
macro_rules! with_regex {
    ($($tokens:tt)*) => {};
}

#[cfg(feature = "alloc")]
extern crate alloc;

#[cfg(feature = "alloc")]
use alloc::{boxed::Box, collections::VecDeque, string::String, vec::Vec};

use core::ops::Deref;

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
        self.as_ref()
    }
}

impl<T: ?Sized> AsRef<T> for RefBox<'_, T> {
    #[inline]
    fn as_ref(&self) -> &T {
        match self {
            RefBox::Ref(r) => r,
            #[cfg(feature = "alloc")]
            RefBox::Box(b) => b,
        }
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

pub trait Blueprinted {
    const BLUEPRINT: &'static Blueprint<'static>;
}

impl<T> Blueprinted for RefBox<'_, T>
where
    T: Blueprinted,
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
    pub schema: BlueprintSchema<'a>,
}

#[derive(Clone, Copy, Debug, PartialEq, PartialOrd)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct IntegerBlueprint {
    /// Minimal value of the number.
    #[cfg_attr(
        feature = "serde",
        serde(
            default = "default_min_integer",
            skip_serializing_if = "is_default_min_integer"
        )
    )]
    pub min: i128,

    /// Maximal value of the number.
    #[cfg_attr(
        feature = "serde",
        serde(
            default = "default_max_integer",
            skip_serializing_if = "is_default_max_integer"
        )
    )]
    pub max: i128,
}

#[derive(Clone, Copy, Debug, PartialEq, PartialOrd)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct RealBlueprint {
    /// Minimal value of the number.
    #[cfg_attr(
        feature = "serde",
        serde(
            default = "default_min_real",
            skip_serializing_if = "is_default_min_real"
        )
    )]
    pub min: f64,

    /// Maximal value of the number.
    #[cfg_attr(
        feature = "serde",
        serde(
            default = "default_max_real",
            skip_serializing_if = "is_default_max_real"
        )
    )]
    pub max: f64,
}

#[derive(Clone, Debug, PartialEq, PartialOrd)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct StringBlueprint<#[cfg(feature = "regex")] 'a> {
    #[cfg(feature = "regex")]
    #[cfg_attr(
        feature = "serde",
        serde(default, skip_serializing_if = "Option::is_none")
    )]
    pub regex: Option<RefBox<'a, str>>,

    #[cfg_attr(
        feature = "serde",
        serde(
            default = "default_min_size",
            skip_serializing_if = "is_default_min_size"
        )
    )]
    pub min_len: usize,

    #[cfg_attr(
        feature = "serde",
        serde(
            default = "default_max_size",
            skip_serializing_if = "is_default_max_size"
        )
    )]
    pub max_len: usize,
}

#[derive(Clone, Debug, PartialEq, PartialOrd)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct SequenceBlueprint<'a> {
    pub element: RefBox<'a, Blueprint<'a>>,

    #[cfg_attr(
        feature = "serde",
        serde(
            default = "default_min_size",
            skip_serializing_if = "is_default_min_size"
        )
    )]
    pub min_len: usize,

    #[cfg_attr(
        feature = "serde",
        serde(
            default = "default_max_size",
            skip_serializing_if = "is_default_max_size"
        )
    )]
    pub max_len: usize,
}

#[derive(Clone, Debug, PartialEq, PartialOrd)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct MappingBlueprint<'a> {
    pub key: RefBox<'a, Blueprint<'a>>,

    pub value: RefBox<'a, Blueprint<'a>>,

    #[cfg_attr(
        feature = "serde",
        serde(
            default = "default_min_size",
            skip_serializing_if = "is_default_min_size"
        )
    )]
    pub min_size: usize,

    #[cfg_attr(
        feature = "serde",
        serde(
            default = "default_max_size",
            skip_serializing_if = "is_default_max_size"
        )
    )]
    pub max_size: usize,
}

#[derive(Clone, Debug, PartialEq, PartialOrd)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct TupleBlueprint<'a> {
    #[cfg_attr(
        feature = "serde",
        serde(
            default = "default_refbox_slice",
            skip_serializing_if = "<[_]>::is_empty"
        )
    )]
    pub elements: RefBox<'a, [RefBox<'a, Blueprint<'a>>]>,
}

#[derive(Clone, Debug, PartialEq, PartialOrd)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct StructBlueprint<'a> {
    #[cfg_attr(
        feature = "serde",
        serde(
            default = "default_refbox_slice",
            skip_serializing_if = "<[_]>::is_empty"
        )
    )]
    pub fields: RefBox<'a, [FieldBlueprint<'a>]>,
}

#[derive(Clone, Debug, PartialEq, PartialOrd)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct EnumBlueprint<'a> {
    #[cfg_attr(
        feature = "serde",
        serde(
            default = "default_refbox_slice",
            skip_serializing_if = "<[_]>::is_empty"
        )
    )]
    pub variants: RefBox<'a, [VariantBlueprint<'a>]>,
}

/// Different kinds of blueprints.
#[derive(Clone, Debug, PartialEq, PartialOrd)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum BlueprintSchema<'a> {
    /// Blueprint of the unit type.
    Unit,

    /// Blueprint of the integer numeric type.
    Integer(IntegerBlueprint),

    /// Blueprint of the real numeric type.
    Real(RealBlueprint),

    /// Blueprint of the string type.
    #[cfg(feature = "regex")]
    String(StringBlueprint<'a>),

    /// Blueprint of the string type.
    #[cfg(not(feature = "regex"))]
    String(StringBlueprint),

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

impl BlueprintSchema<'_> {
    pub fn is_integer(&self) -> bool {
        core::matches!(self, BlueprintSchema::Integer { .. })
    }
    pub fn is_real(&self) -> bool {
        core::matches!(self, BlueprintSchema::Integer { .. })
    }
    pub fn is_numeric(&self) -> bool {
        core::matches!(
            self,
            BlueprintSchema::Integer { .. } | BlueprintSchema::Real { .. }
        )
    }
    pub fn is_string(&self) -> bool {
        core::matches!(self, BlueprintSchema::String { .. })
    }
}

/// Blueprint for named field.
#[derive(Clone, Debug, PartialEq, PartialOrd)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct FieldBlueprint<'a> {
    pub name: RefBox<'a, str>,
    pub blueprint: RefBox<'a, Blueprint<'a>>,
}

/// Blueprint for enum variant.
#[derive(Clone, Debug, PartialEq, PartialOrd)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct VariantBlueprint<'a> {
    pub name: RefBox<'a, str>,
    pub schema: VariantBlueprintSchema<'a>,
}

/// Blueprint for named field.
#[derive(Clone, Debug, PartialEq, PartialOrd)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum VariantBlueprintSchema<'a> {
    /// Blueprint of the unit enum variant.
    Unit,

    /// Blueprint of the tuple enum variant.
    Tuple(TupleBlueprint<'a>),

    /// Blueprint of the struct enum variant.
    Struct(StructBlueprint<'a>),
}

macro_rules! for_sized {
    ($any:ident $(<$($t:ident)+>)? { schema: $schema:expr }) => {
        impl $(<$($t: Blueprinted)+>)? Blueprinted for $any $(<$($t)+>)? {
            const BLUEPRINT: &'static Blueprint<'static> = &Blueprint {
                name: RefBox::Ref(stringify!($any)),
                schema: $schema,
            };
        }
    };
}

macro_rules! for_integers {
    ($integer:ident) => {
        for_sized!($integer {
            schema: BlueprintSchema::Integer(IntegerBlueprint {
                min: $integer::MIN as _,
                max: $integer::MAX as _,
            })
        });
    };
}

macro_rules! for_real {
    ($real:ident) => {
        for_sized!($real {
            schema: BlueprintSchema::Real(RealBlueprint {
                min: f64::NEG_INFINITY,
                max: f64::INFINITY,
            })
        });
    };
}

macro_rules! for_string {
    ($string:ident) => {
        for_sized!($string {
            schema: BlueprintSchema::String(StringBlueprint {
                #[cfg(feature = "regex")]
                regex: None,
                min_len: 0,
                max_len: usize::MAX,
            })
        });
    };
}

macro_rules! for_sequence {
    ($sequence:ident<T>) => {
        for_sized!($sequence<T> {
            schema: BlueprintSchema::Sequence(SequenceBlueprint {
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

for_real!(f32);
for_real!(f64);

for_string!(str);

impl<T> Blueprinted for [T]
where
    T: Blueprinted,
{
    const BLUEPRINT: &'static Blueprint<'static> = &Blueprint {
        name: RefBox::Ref("slice"),
        schema: BlueprintSchema::Sequence(SequenceBlueprint {
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
        schema: BlueprintSchema::Sequence(SequenceBlueprint {
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
                schema: BlueprintSchema::Unit,
            };
        }

    };

    (impl $($a:ident)+) => {
        impl<$($a, )+> Blueprinted for ($($a,)+)
        where $($a: Blueprinted,)+
        {
            const BLUEPRINT: &'static Blueprint<'static> = &Blueprint {
                name: RefBox::Ref("tuple"),
                schema: BlueprintSchema::Tuple( TupleBlueprint { elements: RefBox::Ref(&[ $( RefBox::Ref($a::BLUEPRINT), )+ ]) } ),
            };
        }
    };
}

for_tuple!();

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
fn is_default_min_integer(min: &i128) -> bool {
    *min == default_min_integer()
}

#[inline]
fn is_default_max_integer(max: &i128) -> bool {
    *max == default_max_integer()
}

#[inline]
fn is_default_min_real(min: &f64) -> bool {
    *min == default_min_real()
}

#[inline]
fn is_default_max_real(max: &f64) -> bool {
    *max == default_max_real()
}

#[inline]
fn is_default_min_size(min: &usize) -> bool {
    *min == default_min_size()
}

#[inline]
fn is_default_max_size(max: &usize) -> bool {
    *max == default_max_size()
}

#[inline]
fn default_min_integer() -> i128 {
    i128::MIN
}

#[inline]
fn default_max_integer() -> i128 {
    i128::MAX
}

#[inline]
fn default_min_real() -> f64 {
    f64::NEG_INFINITY
}

#[inline]
fn default_max_real() -> f64 {
    f64::INFINITY
}

#[inline]
fn default_min_size() -> usize {
    0
}

#[inline]
fn default_max_size() -> usize {
    usize::MAX
}

#[inline]
fn default_refbox_slice<'a, T>() -> RefBox<'a, [T]> {
    RefBox::Ref(&[])
}

#[inline]
fn default_refbox_str<'a>() -> RefBox<'a, str> {
    RefBox::Ref("")
}
