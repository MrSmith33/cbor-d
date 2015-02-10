/**
Copyright: Copyright (c) 2014 Andrey Penechko.
License: a$(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors: Andrey Penechko.

Some code is based on msgpack-d by Masahiro Nakagawa.

Concise Binary Object Representation (CBOR) for D lang.

The Concise Binary Object Representation (CBOR) is a data format
whose design goals include the possibility of extremely small code
size, fairly small message size, and extensibility without the need
for version negotiation.  These design goals make it different from
earlier binary serializations such as ASN.1 and MessagePack.

Standards: Conforms to RFC 7049.
*/

module cbor;

private import std.string : format;
private import std.traits;
private import std.typecons : Flag, TypeTuple;
private import std.range : ElementEncodingType;
private import std.conv : to;
private import std.utf : byChar;

//version = Cbor_Debug;

/// Thrown in a case of decoding error.
class CborException : Exception
{
	@trusted pure this(string message, string file = __FILE__, size_t line = __LINE__)
	{
		super(message, file, line);
	}
}

//------------------------------------------------------------------------------
//		 SSSSS  TTTTTTT  OOOOO  RRRRRR    AAA     GGGG  EEEEEEE 
//		SS        TTT   OO   OO RR   RR  AAAAA   GG  GG EE      
//		 SSSSS    TTT   OO   OO RRRRRR  AA   AA GG      EEEEE   
//		     SS   TTT   OO   OO RR  RR  AAAAAAA GG   GG EE      
//		 SSSSS    TTT    OOOO0  RR   RR AA   AA  GGGGGG EEEEEEE 
//------------------------------------------------------------------------------

/// Tagged union for CBOR items.
align(1) struct CborValue
{
	align(1):
	enum Type : ubyte
	{
		boolean,
		nil,
		undefined,

		posinteger,
		neginteger,
		floating,
		
		array,
		map,
		
		raw,
		text,
	}

	static union Via
	{
		bool boolean;
		long integer;
		ulong uinteger;
		double floating;
		
		CborValue[] array;
		CborValue[CborValue] map;
		
		ubyte[] raw;
		string text;
	}

	Type type;
	Via via;

	/**
	 * Constructs a $(D CborValue) with arguments.
	 *
	 * Params:
	 *  value = the real content.
	 *  type  = the type of value.
	 */
	@safe
	this(Type type = Type.nil)
	{
		this.type = type;
	}

	@safe
	this(typeof(null))
	{
		this(Type.nil);
	}

	/// ditto
	@trusted
	this(bool value, Type type = Type.boolean)
	{
		this(type);
		via.boolean = value;
	}


	/// ditto
	@trusted
	this(T)(T value) if (isIntegral!T)
	{
		if (value < 0)
		{
			this(Type.neginteger);
			via.integer = value;
		}
		else
		{
			this(Type.posinteger);
			via.uinteger = value;
		}
	}


	/// ditto
	@trusted
	this(T)(T value, Type type = Type.floating) if (isFloatingPoint!T)
	{
		this(type);
		via.floating = value;
	}


	/// ditto
	@trusted
	this(CborValue[] value, Type type = Type.array)
	{
		this(type);
		via.array = value;
	}


	/// ditto
	@trusted
	this(CborValue[CborValue] value, Type type = Type.map)
	{
		this(type);
		via.map = value;
	}


	/// ditto
	@trusted
	this(ubyte[] value, Type type = Type.raw)
	{
		this(type);
		via.raw = value;
	}

	/// ditto
	@trusted
	this(string value, Type type = Type.text)
	{
		this(type);
		via.text = value;
	}


	/**
	 * Converts value to $(D_PARAM T) type.
	 *
	 * Returns:
	 *  converted value.
	 *
	 * Throws:
	 *  CborException if type is mismatched.
	 *
	 * NOTE:
	 *  Current implementation uses cast.
	 */
	@property @trusted
	T as(T)() if (is(Unqual!T == bool))
	{
		if (type != Type.boolean)
			onCastErrorToFrom!T(type);

		return via.boolean;
	}


	/// ditto
	@property @trusted
	T as(T)() if (isIntegral!T && !is(Unqual!T == enum))
	{
		if (type == Type.neginteger)
			return cast(T)via.integer;

		if (type == Type.posinteger)
			return cast(T)via.uinteger;

		onCastErrorToFrom!T(type);

		assert(false);
	}

	/// ditto
	@property @trusted
	T as(T)() if (isSomeChar!T && !is(Unqual!T == enum))
	{
		if (type == Type.posinteger)
			return cast(T)via.uinteger;

		onCastErrorToFrom!T(type);

		assert(false);
	}


	/// ditto
	@property @trusted
	T as(T)() if (isFloatingPoint!T && !is(Unqual!T == enum))
	{
		if (type != Type.floating)
			onCastErrorToFrom!T(type);

		return cast(T)via.floating;
	}


	/// ditto
	@property @trusted
	T as(T)() if (is(Unqual!T == enum))
	{
		return cast(T)as!(OriginalType!T);
	}


	/// ditto
	@property @trusted
	T as(T)() if (isArray!T && !is(Unqual!T == enum))
	{
		if (type == Type.nil)
		{
			static if (isDynamicArray!T)
			{
				return null;
			}
			else
			{
				return T.init;
			}
		}

		static if (is(Unqual!(ElementType!T) == ubyte))
		{
			if (type != Type.raw)
				onCastErrorToFrom!T(type);

			static if (isDynamicArray!T)
			{
				return cast(T)via.raw;
			}
			else
			{
				if (via.raw.length != T.length)
					onCastErrorToFrom!T(type);

				return cast(T)(via.raw[0 .. T.length]);
			}
		}
		else static if(isSomeChar!(Unqual!(ElementEncodingType!T)))
		{
			if (type != Type.text)
				onCastErrorToFrom!T(type);

			static if (isDynamicArray!T)
			{
				return via.text.to!T;
			}
			else
			{
				if (via.text.length != T.length)
					onCastErrorToFrom!T(type);

				static if (is(Unqual!(ElementEncodingType!T) == char))
				{
					return cast(T)via.text[0 .. T.length];
				}
				else
				{
					alias V = Unqual!(ElementEncodingType!T);
					V[] array = via.text.to!(V[]);
					return cast(T)array[0 .. T.length];
				}
			}
		}
		else
		{
			alias V = Unqual!(ElementType!T);

			if (type != Type.array)
				onCastErrorToFrom!T(type);
			static if (isDynamicArray!T)
			{
				V[] array = new V[via.array.length];
			}
			else
			{
				T array = void;
			}

			foreach (i, elem; via.array)
				array[i] = elem.as!(V);

			return cast(T)array;
		}
	}


	/// ditto
	@property @trusted
	T as(T)() if (isAssociativeArray!T)
	{
		alias K = typeof(T.init.keys[0]);
		alias V = typeof(T.init.values[0]);

		if (type == Type.nil)
			return null;

		if (type != Type.map)
			onCastErrorToFrom!T(type);

		V[K] map;

		foreach (key, value; via.map)
			map[key.as!(K)] = value.as!(V);

		return map;
	}


	/// ditto
	@property @trusted
	T as(T)()
		if (is(T == struct) || is(T == class) || isTuple!T)
	{
		static if (is(T == class))
			if (type == CborValue.Type.nil)
				return null;

		if (type != CborValue.Type.array)
			throw new CborException(format("Can not decode %s from %s", T.stringof, type));

		T obj;

		size_t arrLength = via.array.length;
		size_t numMembers;

		static if (isTuple!T)
			numMembers = T.Types.length;
		else
			numMembers = numEncodableMembers!T;

		if (arrLength != numMembers)
		{
			throw new CborException(
				format("The number of deserialized members of %s is mismatched."~
					" Got %s, while expected %s members",
						T.stringof, numMembers, arrLength));
		}

		static if (isTuple!T)
		{
			foreach (i, Type; T.Types)
				obj.field[i] = via.array[i].as!(Type);
		}
		else 
		{  // simple struct
			static if (is(T == class))
				obj = new T();

			foreach(i, ref member; obj.tupleof)
			{
				static if (isEncodedField!(typeof(member)))
					member = via.array[i].as!(typeof(member));
			}
		}

		return obj;
	}


	/// Comparison for equality.
	@trusted
	bool opEquals()(auto ref const CborValue other) const
	{
		if (type != other.type)
			return false;
	
		final switch(other.type)
		{
			case Type.boolean: return opEquals(other.via.boolean);
			case Type.nil: return type == Type.nil;
			case Type.undefined: return type == Type.undefined;
			case Type.neginteger: return opEquals(other.via.integer);
			case Type.posinteger: return opEquals(other.via.uinteger);
			case Type.floating: return opEquals(other.via.floating);
			case Type.array: return opEquals(other.via.array);
			case Type.map: return opEquals(other.via.map);
			case Type.raw: return opEquals(other.via.raw);
			case Type.text: return opEquals(other.via.text);
		}
	}


	/// ditto
	@trusted
	bool opEquals(T : bool)(const T other) const
	{
		if (type != Type.boolean)
			return false;

		return via.boolean == other;
	}


	/// ditto
	@trusted
	bool opEquals(T)(const T other) const if (isIntegral!T && !is(T == typeof(null)))
	{
		static if (isUnsigned!T)
		{
			if (type == Type.posinteger)
				return via.uinteger == other;
			else
				return false;
		}
		else
		{
			if (type == Type.neginteger || type == Type.posinteger)
				return via.integer == other;
			else
				return false;
		}
	}


	/// ditto
	@trusted
	bool opEquals(T)(const T other) const if (isFloatingPoint!T)
	{
		if (type != Type.floating)
			return false;

		return via.floating == other;
	}


	/// ditto
	@trusted
	bool opEquals(T)(const typeof(null) other) const if (is(T == typeof(null)))
	{
		if (type == Type.array || type == Type.raw || type == Type.text)
		{
			return via.raw.length == 0;
		}
		else if (type == Type.map)
		{
			return via.map.length == 0;
		}

		return false;
	}


	/// ditto
	@trusted
	bool opEquals(T : const CborValue[])(const T other) const if (!is(T == typeof(null)))
	{
		if (type != Type.array)
			return false;

		return via.array == other;
	}

	/// ditto
	@trusted
	bool opEquals(T : const(V)[], V)(const T other) const if (!is(T == typeof(null)))
	{
		if (type != Type.array)
			return false;

		if (other.length != via.array.length) return false;

		auto arr = via.array;

		foreach(i, ref item; other)
		{
			if (item != arr[i]) return false;
		}

		return true;
	}


	/// ditto
	@trusted
	bool opEquals(T : const CborValue[CborValue])(const T other) const if (!is(T == typeof(null)))
	{
		if (type != Type.map)
			return false;

		// This comparison is instead of default comparison because 'via.map == other' raises "Access Violation".
		foreach (key, value; via.map) {
			if (key in other) {
				if (other[key] != value)
					return false;
			} else {
				return false;
			}
		}

		return true;
	}

	/// ditto
	@trusted
	bool opEquals(T : const V[K], K, V)(const T other) const if (!is(T == typeof(null)))
	{
		if (type != Type.map)
			return false;

		// This comparison is instead of default comparison because 'via.map == other' raises "Access Violation".
		auto map = via.map;

		if (map.length != other.length) return false;

		foreach (key, value; other) {
			if (auto thisVal = CborValue(key) in map) {
				if (*thisVal != value)
					return false;
			} else {
				return false;
			}
		}

		return true;
	}


	/// ditto
	@trusted
	bool opEquals(T : const(ubyte)[])(const T other) const if (!is(T == typeof(null)))
	{
		if (type != Type.raw)
			return false;

		return via.raw == other;
	}


	/// ditto
	@trusted
	bool opEquals(T : string)(const T other) const if (!is(T == typeof(null)))
	{
		if (type != Type.text)
			return false;

		return via.text == other;
	}

	/// Hashing.
	size_t toHash() const nothrow @trusted
	{
		final switch(type)
		{
			case Type.boolean: return typeid(bool).getHash(&via.boolean);
			case Type.nil: return 0;
			case Type.undefined: return size_t.max;
			case Type.neginteger: return typeid(long).getHash(&via.integer);
			case Type.posinteger: return typeid(ulong).getHash(&via.uinteger);
			case Type.floating: return typeid(real).getHash(&via.floating);
			case Type.array: return typeid(CborValue[]).getHash(&via.array);
			case Type.map: return typeid(CborValue[CborValue]).getHash(&via.map);
			case Type.raw: return typeid(ubyte[]).getHash(&via.raw);
			case Type.text: return typeid(string).getHash(&via.text);
		}
	}

	/// String representation.
	string toString()
	{
		import std.string : format;
		final switch(type)
		{
			case Type.boolean: return format("CborValue(%s)", via.boolean);
			case Type.nil: return "CborValue(null)";
			case Type.undefined: return "CborValue(undefined)";
			case Type.neginteger: return format("CborValue(%s, %s)", type, via.integer);
			case Type.posinteger: return format("CborValue(%s, %s)", type, via.uinteger);
			case Type.floating: return format("CborValue(%s, %s)", type, via.floating);
			case Type.array: return format("CborValue(%s, %s)", type, via.array);
			case Type.map: return format("CborValue(%s, %s)", type, via.map);
			case Type.raw: return format("CborValue(%s, %s)", type, via.raw);
			case Type.text: return format("CborValue(%s, \"%s\")", type, via.text);
		}
	}
}

//------------------------------------------------------------------------------
//		EEEEEEE NN   NN  CCCCC   OOOOO  DDDDD   IIIII NN   NN   GGGG  
//		EE      NNN  NN CC    C OO   OO DD  DD   III  NNN  NN  GG  GG 
//		EEEEE   NN N NN CC      OO   OO DD   DD  III  NN N NN GG      
//		EE      NN  NNN CC    C OO   OO DD   DD  III  NN  NNN GG   GG 
//		EEEEEEE NN   NN  CCCCC   OOOO0  DDDDDD  IIIII NN   NN  GGGGGG 
//------------------------------------------------------------------------------


private import std.range : isInputRange, isOutputRange, ElementType;
private import std.typecons : isTuple;

/// Encodes value E into output range sink.
/// Returns number of bytes written to sink.
size_t encodeCbor(R, E)(auto ref R sink, E value)
	if(isOutputRange!(R, ubyte))
{
	import std.typecons : isTuple;

	static if (isIntegral!E)
	{
		return encodeCborInt(sink, value);
	}
	else static if (isSomeChar!E)
	{
		return encodeCborInt(sink, cast(ulong)value);
	}
	else static if (isFloatingPoint!E)
	{
		return encodeCborFloat(sink, value);
	}
	else static if (isBoolean!E)
	{
		return encodeCborBool(sink, value);
	}
	else static if (is(Unqual!E == typeof(null)))
	{
		return encodeCborNull(sink, value);
	}
	else static if ((isArray!E || isInputRange!E) && is(Unqual!(ElementType!E) == ubyte))
	{
		return encodeCborRaw(sink, value);
	}
	else static if ((isArray!E || isInputRange!E) && isSomeChar!(Unqual!(ElementEncodingType!E)))
	{
		return encodeCborString(sink, value);
	}
	else static if (isInputRange!E || isArray!E || isTuple!E ||
		is(E == class) || is(E == struct))
	{
		return encodeCborArray(sink, value);
	}
	else static if (isAssociativeArray!E)
	{
		return encodeCborMap(sink, value);
	}
	else
	{
		static assert(false, "Unable to encode " ~ E.stringof);
	}
}

// Encode integer types as separate type or as part of arrays or map.
private size_t encodeLongType(R)(auto ref R sink, ubyte majorType, ulong length)
	if(isOutputRange!(R, ubyte))
{
	import std.bitmanip : nativeToBigEndian;
	import std.array;

	majorType <<= 5;
	if (length < 24) {
		putChecked(sink, cast(ubyte)(majorType | length));
		return 1;
	} else if (length <= ubyte.max) {
		putChecked(sink, cast(ubyte)(majorType | 24));
		putChecked(sink, cast(ubyte)length);
		return 2;
	} else if (length <= ushort.max) {
		putChecked(sink, cast(ubyte)(majorType | 25));
		putChecked(sink, nativeToBigEndian!ushort(cast(ushort)length)[]);
		return 3;
	} else if (length <= uint.max) {
		putChecked(sink, cast(ubyte)(majorType | 26));
		putChecked(sink, nativeToBigEndian!uint(cast(uint)length)[]);
		return 5;
	} else { // if (length <= ulong.max)
		putChecked(sink, cast(ubyte)(majorType | 27));
		putChecked(sink, nativeToBigEndian!ulong(cast(ulong)length)[]);
		return 9;
	}
}

/// Encodes integer.
size_t encodeCborInt(R, E)(auto ref R sink, E value)
	if(isOutputRange!(R, ubyte) && isIntegral!E)
{
	ulong val;
	ubyte majorType;
	
	if (value < 0) {
		val = -value - 1;
		majorType = 1;
	} else {
		val = value;
		majorType = 0;
	}

	return encodeLongType(sink, majorType, val);
}

/// Encodes floating.
size_t encodeCborFloat(R, E)(auto ref R sink, E value)
	if(isOutputRange!(R, ubyte) && isFloatingPoint!E)
{
	import std.bitmanip : nativeToBigEndian;
	enum majorType = 7 << 5;


	static if (is(Unqual!E == float))
	{
		__FloatRep flt;
		flt.f = value;
		putChecked(sink, cast(ubyte)(majorType | 26));
		putChecked(sink, nativeToBigEndian!uint(flt.u)[]);
		return 5;
	}
	else static if (is(Unqual!E == double) || is(Unqual!E == real))
	{
		__DoubleRep dbl;
		dbl.d = cast(double)value;
		putChecked(sink, cast(ubyte)(majorType | 27));
		putChecked(sink, nativeToBigEndian!ulong(dbl.u)[]);
		return 9;
	}
}

/// Encodes boolean.
size_t encodeCborBool(R, E)(auto ref R sink, E value)
	if(isOutputRange!(R, ubyte) && isBoolean!E)
{
	if (value)
		putChecked(sink, cast(ubyte)0xf5);
	else
		putChecked(sink, cast(ubyte)0xf4);
	return 1;
}

/// Encodes null.
size_t encodeCborNull(R, E)(auto ref R sink, E value)
	if(isOutputRange!(R, ubyte) && is(Unqual!E == typeof(null)))
{
	putChecked(sink, cast(ubyte)0xf6);
	return 1;
}

/// Encodes range of ubytes.
size_t encodeCborRaw(R, E)(auto ref R sink, E value)
	if(isOutputRange!(R, ubyte) &&
		(isArray!E || isInputRange!E) && is(Unqual!(ElementType!E) == ubyte))
{
	auto size = encodeLongType(sink, 2, value.length);
	size += value.length;
	putChecked(sink, value[]);
	return size;
}

/// Encodes string.
size_t encodeCborString(R, E)(auto ref R sink, E value)
	if(isOutputRange!(R, ubyte) && isSomeChar!(Unqual!(ElementEncodingType!E)))
{
	auto size = encodeLongType(sink, 3, value.length);
	size += value.length;
	static if (is(Unqual!(ElementEncodingType!E) == char))
	{
		putChecked(sink, cast(ubyte[])value);
	}
	else
	{
		foreach(char elem; value[].byChar)
		{
			putChecked(sink, cast(ubyte)elem);
		}
	}
	return size;
}

/// Encodes array of any items or a tuple as cbor array.
size_t encodeCborArray(R, E)(auto ref R sink, E value)
	if(isOutputRange!(R, ubyte) &&
	(isInputRange!E || isArray!E || isTuple!E))
{
	static if (isArray!E && is(Unqual!(ElementType!E) == void)) // accept []
	{
		return encodeCborArrayHead(sink, 0);
	}
	else
	{
		auto size = encodeCborArrayHead(sink, value.length);
		foreach(item; value)
			size += encodeCbor(sink, item);
		return size;
	}
}

/// Encodes structs and classes as cbor array.
size_t encodeCborArray(R, A)(auto ref R sink, A aggregate)
	if(isOutputRange!(R, ubyte) &&
		(is(A == struct) || is(A == class)) &&
		!isTuple!A)
{
	return encodeCborAggregate!(Flag!"WithFieldName".no)(sink, aggregate);
}

/// Encodes asociative array as cbor map.
size_t encodeCborMap(R, E)(auto ref R sink, E value)
	if(isOutputRange!(R, ubyte) && isAssociativeArray!E)
{
	auto size = encodeCborMapHead(sink, value.length);
	foreach(key, element; value)
	{
		size += encodeCbor(sink, key);
		size += encodeCbor(sink, element);
	}
	return size;
}

/// Encodes structs and classes as cbor map.
/// Note, that decoding of structs and classes from maps is not supported (yet).
size_t encodeCborMap(R, A)(auto ref R sink, A aggregate)
	if(isOutputRange!(R, ubyte) &&
		(is(A == struct) || is(A == class)) &&
		!isTuple!A)
{
	return encodeCborAggregate!(Flag!"WithFieldName".yes)(sink, aggregate);
}

/// Encode array head with arrayLength elements.
/// arrayLength items must follow.
size_t encodeCborArrayHead(R)(auto ref R sink, ulong arrayLength)
	if(isOutputRange!(R, ubyte))
{
	return encodeLongType(sink, 4, arrayLength);
}

/// Encode map head with mapLength elements.
/// mapLength pairs of items must follow. Keys first, then values.
size_t encodeCborMapHead(R)(auto ref R sink, ulong mapLength)
	if(isOutputRange!(R, ubyte))
{
	return encodeLongType(sink, 5, mapLength);
}

/// Encodes classes and structs. If withFieldName is yes, than value is encoded as map.
/// If withFieldName is no, then value is encoded as an array.
size_t encodeCborAggregate(Flag!"WithFieldName" withFieldName, R, A)(auto ref R sink, auto ref A aggregate)
	if (isOutputRange!(R, ubyte) && (is(A == struct) || is(A == class)))
{
	size_t size;
	static if (is(A == class))
		if (aggregate is null)
			return encodeCbor(sink, null);

	size += encodeCborArrayHead(sink, numEncodableMembers!A);
	foreach(i, member; aggregate.tupleof)
	{
		static if (isEncodedField!(typeof(member)))
		{
			static if (withFieldName)
				size += encodeCborString(sink, __traits(identifier, aggregate.tupleof[i]));
			size += encodeCbor(sink, member);
		}
	}
	return size;
}

//------------------------------------------------------------------------------
//		DDDDD   EEEEEEE  CCCCC   OOOOO  DDDDD   IIIII NN   NN   GGGG  
//		DD  DD  EE      CC    C OO   OO DD  DD   III  NNN  NN  GG  GG 
//		DD   DD EEEEE   CC      OO   OO DD   DD  III  NN N NN GG      
//		DD   DD EE      CC    C OO   OO DD   DD  III  NN  NNN GG   GG 
//		DDDDDD  EEEEEEE  CCCCC   OOOO0  DDDDDD  IIIII NN   NN  GGGGGG 
//------------------------------------------------------------------------------


/// Decodes single value and returns it as CborValue tagged union type.
/// Throws CborException if data is not well-formed.
/// Note, that ubyte[] and string types are slices of input range if ubyte[] was provided.
/// Will modify input range, popping all the bytes of the first item.
CborValue decodeCbor(R)(auto ref R input)
	if(isInputRange!R && is(ElementType!R == ubyte))
{
	import std.array;
	import std.bitmanip;
	
	// tags will be ignored and decoding will restart from here
	start_label:

	if (input.empty) onInsufficientInput();

	ubyte item = input.front;
	input.popFront;

	switch(item)
	{
		case 0x00: .. case 0x17: // Integer 0x00..0x17 (0..23)
			return CborValue(item);
		case 0x18: // Unsigned integer (one-byte uint8_t follows)
			return CborValue(readInteger!ubyte(input));
		case 0x19: // Unsigned integer (two-byte uint16_t follows)
			return CborValue(readInteger!ushort(input));
		case 0x1a: // Unsigned integer (four-byte uint_t follows)
			return CborValue(readInteger!uint(input));
		case 0x1b: // Unsigned integer (eight-byte uint64_t follows)
			return CborValue(readInteger!ulong(input));
		case 0x20: .. case 0x37: // Negative integer -1-0x00..-1-0x17 (-1..-24)
			return CborValue(cast(byte)(-1 - item + 0x20));
		case 0x38: // Negative integer -1-n (one-byte uint8_t for n follows)
			return CborValue(-1 - cast(long)readInteger!ubyte(input));
		case 0x39: // Negative integer -1-n (two-byte uint16_t for n follows)
			return CborValue(-1 - cast(long)readInteger!ushort(input));
		case 0x3a: // Negative integer -1-n (four-byte uint_t for n follows)
			return CborValue(-1 - cast(long)readInteger!uint(input));
		case 0x3b: // Negative integer -1-n (eight-byte uint64_t for n follows)
			return CborValue(-1 - cast(long)readInteger!ulong(input));
		case 0x40: .. case 0x57: // byte string (0x00..0x17 bytes follow)
			return CborValue(readBytes(input, item - 0x40));
		case 0x58: // byte string (one-byte uint8_t for n, and then n bytes follow)
			return CborValue(readBytes(input, readInteger!ubyte(input)));
		case 0x59: // byte string (two-byte uint16_t for n, and then n bytes follow)
			return CborValue(readBytes(input, readInteger!ushort(input)));
		case 0x5a: // byte string (four-byte uint_t for n, and then n bytes follow)
			return CborValue(readBytes(input, readInteger!uint(input)));
		case 0x5b: // byte string (eight-byte uint64_t for n, and then n bytes follow)
			return CborValue(readBytes(input, readInteger!ulong(input)));
		case 0x5f: // byte string, byte strings follow, terminated by "break"
			onUnsupportedTag(item); break;
		case 0x60: .. case 0x77: // UTF-8 string (0x00..0x17 bytes follow)
			return CborValue(cast(string)readBytes(input, item - 0x60));
		case 0x78: // UTF-8 string (one-byte uint8_t for n, and then n bytes follow)
			return CborValue(cast(string)readBytes(input, readInteger!ubyte(input)));
		case 0x79: // UTF-8 string (two-byte uint16_t for n, and then n bytes follow)
			return CborValue(cast(string)readBytes(input, readInteger!ushort(input)));
		case 0x7a: // UTF-8 string (four-byte uint_t for n, and then n bytes follow)
			return CborValue(cast(string)readBytes(input, readInteger!uint(input)));
		case 0x7b: // UTF-8 string (eight-byte uint64_t for n, and then n bytes follow)
			return CborValue(cast(string)readBytes(input, readInteger!ulong(input)));
		case 0x7f: // UTF-8 string, UTF-8 strings follow, terminated by "break"
			onUnsupportedTag(item); break;
		case 0x80: .. case 0x97: // array (0x00..0x17 data items follow)
			return CborValue(readArray(input, item - 0x80));
		case 0x98: // array (one-byte uint8_t for n, and then n data items follow)
			return CborValue(readArray(input, readInteger!ubyte(input)));
		case 0x99: // array (two-byte uint16_t for n, and then n data items follow)
			return CborValue(readArray(input, readInteger!ushort(input)));
		case 0x9a: // array (four-byte uint_t for n, and then n data items follow)
			return CborValue(readArray(input, readInteger!uint(input)));
		case 0x9b: // array (eight-byte uint64_t for n, and then n data items follow)
			return CborValue(readArray(input, readInteger!ulong(input)));
		case 0x9f: // array, data items follow, terminated by "break"
			onUnsupportedTag(item); break;
		case 0xa0: .. case 0xb7: // map (0x00..0x17 pairs of data items follow)
			return CborValue(readMap(input, item - 0xa0));
		case 0xb8: // map (one-byte uint8_t for n, and then n pairs of data items follow)   
			return CborValue(readMap(input, readInteger!ubyte(input)));
		case 0xb9: // map (two-byte uint16_t for n, and then n pairs of data items follow)
			return CborValue(readMap(input, readInteger!ushort(input)));
		case 0xba: // map (four-byte uint_t for n, and then n pairs of data items follow)
			return CborValue(readMap(input, readInteger!uint(input)));
		case 0xbb: // map (eight-byte uint64_t for n, and then n pairs of data items follow)
			return CborValue(readMap(input, readInteger!ulong(input)));
		case 0xbf: // map, pairs of data items follow, terminated by "break"
			onUnsupportedTag(item); break;
		case 0xc0: .. case 0xd7: // (tagged item) ignore
			goto start_label;
		case 0xd8: // ignore 1-byte tag
			dropBytes(input, 1);
			goto start_label;
		case 0xd9: // ignore 2-byte tag
			dropBytes(input, 2);
			goto start_label;
		case 0xda: // ignore 4-byte tag
			dropBytes(input, 4);
			goto start_label;
		case 0xdb: // ignore 8-byte tag
			dropBytes(input, 8);
			goto start_label;
		case 0xe0: .. case 0xf3: // (simple value)
			onUnsupportedTag(item); break;
		case 0xf4: // False
			return CborValue(false);
		case 0xf5: // True
			return CborValue(true);
		case 0xf6: // Null
			return CborValue(null);
		case 0xf7: // Undefined
			return CborValue(CborValue.Type.undefined);
		case 0xf8: // (simple value, one byte follows)
			onUnsupportedTag(item); break;
		case 0xf9: // Half-Precision Float (two-byte IEEE 754)
			__HalfRep hr = {u : readInteger!ushort(input)};
			return CborValue(hr.h.get!double);
		case 0xfa: // Single-Precision Float (four-byte IEEE 754)
			__FloatRep fr = {u : readInteger!uint(input)};
			return CborValue(fr.f);
		case 0xfb: // Double-Precision Float (eight-byte IEEE 754)
			__DoubleRep dr = {u : readInteger!ulong(input)};
			return CborValue(dr.d);
		case 0xff: // "break" stop code
			onUnsupportedTag(item); break;
		default:
			onUnsupportedTag(item);
	}

	assert(false);
}

/// Decodes single cbor value and tries to convert it to requested type.
/// If types doesn't match CborException is thrown.
/// Note, that ubyte[] and string types are slices of input range if ubyte[] was provided.
/// Will modify input range, popping all the elements of T.
T decodeCborSingle(T, R)(auto ref R input)
	if(isInputRange!R && is(ElementType!R == ubyte))
{
	CborValue value = decodeCbor(input);
	return value.as!T;
}

/// Decodes single cbor value and tries to convert it to requested type.
/// If types doesn't match CborException is thrown.
/// Note, that this version will dup all arrays for you.
/// Will modify input range, popping all the elements of T.
T decodeCborSingleDup(T, R)(auto ref R input)
	if(isInputRange!R && is(ElementType!R == ubyte))
{
	CborValue value = decodeCbor(input);

	static if (is(T == E[], E))
		return value.as!T.dup;
	else
		return value.as!T;
}

//------------------------------------------------------------------------------
//		HH   HH EEEEEEE LL      PPPPPP  EEEEEEE RRRRRR   SSSSS  
//		HH   HH EE      LL      PP   PP EE      RR   RR SS      
//		HHHHHHH EEEEE   LL      PPPPPP  EEEEE   RRRRRR   SSSSS  
//		HH   HH EE      LL      PP      EE      RR  RR       SS 
//		HH   HH EEEEEEE LLLLLLL PP      EEEEEEE RR   RR  SSSSS  
//------------------------------------------------------------------------------

private void putChecked(R, E)(ref R sink, auto ref E e)
{
	import std.range : put, hasLength;
	version(Cbor_Debug)
	static if (hasLength!R)
	{
		size_t elemSize;

		static if (hasLength!E)
			elemSize = e.length * ElementType!E.sizeof;
		else
			elemSize = E.sizeof;

		assert(sink.length >= elemSize,
			format("Provided sink length is to small. Sink.$ %s < Data.$ %s", sink.length, elemSize));
	}
	put(sink, e);
}

private T readInteger(T, R)(auto ref R input)
{
	return readN!(T.sizeof, T, R)(input);
}

private T readN(ubyte size, T, R)(auto ref R input)
	if(isInputRange!R && is(ElementType!R == ubyte))
{
	import std.algorithm : copy, take;
	import std.bitmanip : bigEndianToNative;
	import std.range : dropExactly;
	
	static assert(T.sizeof == size);
	static assert(size > 0);
	if (input.length < size) onInsufficientInput();

	ubyte[size] data;

	copy(take(input, size), data[]);
	input = input.dropExactly(size);
	T result = bigEndianToNative!(T, size)(data);

	return result;
}

// Drops exactly length bytes from input range.
// If there is not sufficient bytes in input, CborException is thrown.
private void dropBytes(R)(auto ref R input, ulong length)
	if(isInputRange!R && is(ElementType!R == ubyte))
{
	import std.range : dropExactly;
	if (input.length < length) onInsufficientInput();
	input = input.dropExactly(cast(size_t)length);
}

// Reads byte array from input range. On 32-bit can read up to uint.max bytes.
// If ubyte[] is passed as input, a slice will be returned.
// Make sure to dup array when input buffer is reused.
private ubyte[] readBytes(R)(auto ref R input, ulong length)
	if(isInputRange!R && is(ElementType!R == ubyte))
{
	import std.algorithm : take;
	import std.array;
	if (input.length < length) onInsufficientInput();

	static if (size_t.sizeof < ulong.sizeof)
		if (length > size_t.max)
			throw new CborException(format("Array size is too big %s", length));
	
	size_t dataLength = cast(size_t)length;
	ubyte[] result;
	static if (is(R == ubyte[]))
	{
		result = input[0..dataLength];
		input = input[dataLength..$];
	}
	else
	{
		result = take(input, dataLength).array;
	}

	return result;
}

// Read array of 'length' items into CborValue[].
private CborValue[] readArray(R)(auto ref R input, ulong length)
	if(isInputRange!R && is(ElementType!R == ubyte))
{
	static if (size_t.sizeof < ulong.sizeof)
		if (length > size_t.max)
			throw new CborException(format("Array size is too big %s", length));

	size_t arrayLength = cast(size_t)length;
	CborValue[] result = new CborValue[arrayLength]; // can use uninitializedArray
	foreach(ref elem; result)
	{
		elem = decodeCbor(input);
	}

	return result;
}

// length - num of pairs
private CborValue[CborValue] readMap(R)(auto ref R input, ulong length)
	if(isInputRange!R && is(ElementType!R == ubyte))
{
	static if (size_t.sizeof < ulong.sizeof)
		if (length > size_t.max)
			throw new CborException(format("Map size is too big: %s", length));

	size_t mapLength = cast(size_t)length;
	CborValue[CborValue] result; // can use uninitializedArray
	foreach(i; 0..mapLength)
	{
		auto key = decodeCbor(input);
		if (key in result) throw new CborException(format("duplicate key %s in map found", key));
		result[key] = decodeCbor(input);
	}

	return result;
}

private import std.numeric : CustomFloat;
private union __HalfRep { CustomFloat!16 h; ushort u; string toString(){return format("__HalfRep(%s, %x)", h, u);};}
private union __FloatRep { float f; uint u; string toString(){return format("__FloatRep(%s, %x)", f, u);};}
private union __DoubleRep { double d; ulong u; string toString(){return format("__DoubleRep(%s, %x)", d, u);};}

private template isEncodedField(T)
{
	enum isEncodedField = isIntegral!T || isFloatingPoint!T || isBoolean!T ||
		is(Unqual!T == typeof(null)) || isArray!T || isInputRange!T ||
		isTuple!T || is(T == string) || is(T == class) || is(T == struct) ||
		isAssociativeArray!T;
}

/// Returns a number of aggregate members that will be encoded by cbor-d.
template numEncodableMembers(alias T)
{
	enum numEncodableMembers = numEncodableMembersImpl!(T.tupleof);
}

private template numEncodableMembersImpl(members ...)
{
	static if (members.length == 0)
		enum numEncodableMembersImpl = 0;
	else
		enum numEncodableMembersImpl = 
			cast(int)isEncodedField!(typeof(members[0])) +
			numEncodableMembersImpl!(members[1..$]);
}

//------------------------------------------------------------------------------
//		TTTTTTT EEEEEEE  SSSSS  TTTTTTT  SSSSS  
//		  TTT   EE      SS        TTT   SS      
//		  TTT   EEEEE    SSSSS    TTT    SSSSS  
//		  TTT   EE           SS   TTT        SS 
//		  TTT   EEEEEEE  SSSSS    TTT    SSSSS  
//------------------------------------------------------------------------------
                                      

unittest // positive integer
{
	CborValue val = CborValue(22);
	assert(val == 22);
	assert(val.as!ubyte == 22);
	assert(val.as!ushort == 22);
	assert(val.as!uint == 22);
	assert(val.as!ulong == 22);
	assert(val.as!byte == 22);
	assert(val.as!short == 22);
	assert(val.as!int == 22);
	assert(val.as!long == 22);
	assert(val.type == CborValue.Type.posinteger);

	val = CborValue(ulong.max);
	assert(val == ulong.max);
	assert(val.type == CborValue.Type.posinteger);
}

unittest // negative integer
{
	CborValue val = CborValue(-22);
	assert(val == -22);
	assert(val !=  22);
	assert(val.as!byte == -22);
	assert(val.as!short == -22);
	assert(val.as!int == -22);
	assert(val.as!long == -22);
	assert(val.as!ulong == cast(ulong)-22);
	assert(val.type == CborValue.Type.neginteger);
}

unittest // floating point
{
	CborValue val = CborValue(-22.0f);
	assert(val == -22f);
	assert(val.as!real == -22f);
	assert(val.as!double == -22f);
	assert(val.as!float == -22f);
	assert(val.type == CborValue.Type.floating);
}

unittest // boolean
{
	CborValue val = CborValue(true);
	assert(val == true);
	assert(val.as!bool == true);
	assert(val.type == CborValue.Type.boolean);

	val = CborValue(false);
	assert(val == false);
	assert(val.as!bool == false);
	assert(val.type == CborValue.Type.boolean);
}

unittest // undefined
{
	CborValue val = CborValue(CborValue.Type.undefined);
	assert(val == CborValue(CborValue.Type.undefined));
	assert(val.type == CborValue.Type.undefined);
}

unittest // null value
{
	CborValue val = CborValue(null);
	assert(val == CborValue(null)); // prefer
	assert(val.as!(ubyte[]) == null);
	assert(val.as!(ubyte[3]) == [0,0,0]);
	assert(val == CborValue(CborValue.Type.nil));
	assert(val.type == CborValue.Type.nil);
}

unittest // array
{
	CborValue val = CborValue([CborValue(1), CborValue("string"), CborValue(true), CborValue(null)]);
	assert(val == [CborValue(1), CborValue("string"), CborValue(true), CborValue(null)]);
	assert(val.type == CborValue.Type.array);
}

unittest // map
{
	CborValue val = CborValue([CborValue("a") : CborValue(42)]);
	assert(val == [CborValue("a") : CborValue(42)]);
	assert(val.type == CborValue.Type.map);
}

unittest // byte string
{
	CborValue val = CborValue(cast(ubyte[])[1, 2, 3, 4, 5]);
	assert(val == cast(ubyte[])[1, 2, 3, 4, 5]);
	assert(val.type == CborValue.Type.raw);
}

unittest // string
{
	CborValue val = CborValue("hello");
	assert(val == "hello");
	assert(val.type == CborValue.Type.text);
}

// Testing helpers
version(unittest)
{
	private ubyte[1024] buf;

	private ubyte[] getEncoded(T)(T value)
	{
		return buf[0..encodeCbor(buf[], value)];
	}

	private string toHexString(ubyte[] arr)
	{
		return format("0x%(%02x%)", arr);
	}
	private string encodedString(T)(T value)
	{
		return toHexString(getEncoded(value));
	}
	private void printEncoded(T)(T value)
	{
		import std.stdio : writeln;
		encodedString(value).writeln;
	}
	private void cmpEncoded(T)(T value, string encodedStr)
	{
		auto encoded = encodedString(value);
		assert(encoded == encodedStr, format("%s != %s", encoded, encodedStr));
	}
	private void cmpEncodedConst(T)(T value, string encodedStr)
	{
		auto encoded = encodedString(cast(const)value);
		assert(encoded == encodedStr, format("%s != %s", encoded, encodedStr));
	}
	private void cmpEncodedImmutable(T)(T value, string encodedStr)
	{
		auto encoded = encodedString(cast(immutable)value);
		assert(encoded == encodedStr, format("%s != %s", encoded, encodedStr));
	}
}

unittest // encoding
{
	testEncoding!cmpEncoded();
	testEncoding!cmpEncodedConst();
	testEncoding!cmpEncodedImmutable();
}

version(unittest)
private void testEncoding(alias cmpEncoded)()
{
	// Test vectors
	cmpEncoded(0, "0x00");
	cmpEncoded(1, "0x01");
	cmpEncoded(10, "0x0a");
	cmpEncoded(23, "0x17");
	cmpEncoded(24, "0x1818");
	cmpEncoded(25, "0x1819");
	cmpEncoded(100, "0x1864");
	cmpEncoded(1000, "0x1903e8");
	cmpEncoded(1000000, "0x1a000f4240");
	cmpEncoded(1000000000000, "0x1b000000e8d4a51000");

	//bignums
	//cmpEncoded(18446744073709551615, "0x1bffffffffffffffff");
	//cmpEncoded(18446744073709551616, "0xc249010000000000000000");
	//cmpEncoded(-18446744073709551616, "0x3bffffffffffffffff");
	//cmpEncoded(-18446744073709551617, "0xc349010000000000000000");
	cmpEncoded(-1, "0x20");
	cmpEncoded(-10, "0x29");
	cmpEncoded(-100, "0x3863");
	cmpEncoded(-1000, "0x3903e7");
	
	//printEncoded(0.0f, "0xf90000"); // half-float
	//printEncoded(-0.0f, "0xf98000"); // half-float
	//printEncoded(1.0f, "0xf93c00"); // half-float
	cmpEncoded(1.1, "0xfb3ff199999999999a");
	//printEncoded(1.5f, "0xf93e00"); // half-float
	//printEncoded(65504.0f, "0xf97bff"); // half-float
	cmpEncoded(100000.0f, "0xfa47c35000");
	cmpEncoded(3.4028234663852886e+38f, "0xfa7f7fffff");
	cmpEncoded(1.0e+300, "0xfb7e37e43c8800759c");
	//printEncoded(5.960464477539063e-8f, "0xf90001"); // half-float
	//printEncoded(0.00006103515625f, "0xf90400"); // half-float
	//printEncoded(-4.0f, "0xf9c400"); // half-float
	cmpEncoded(-4.1, "0xfbc010666666666666");
	cmpEncoded(1.0f/0, "0xfa7f800000"); // Infinity
	//cmpEncoded(NaN, "0xfa7fc00000"); // produces 0xfa7fe00000
	cmpEncoded(-1.0f/0, "0xfaff800000"); // -Infinity
	cmpEncoded(1.0/0, "0xfb7ff0000000000000"); // Infinity
	//cmpEncoded(NaN, "0xfb7ff8000000000000"); // produces 0xfb7ffc000000000000
	cmpEncoded(-1.0/0, "0xfbfff0000000000000");// -Infinity

	cmpEncoded(false, "0xf4");
	cmpEncoded(true, "0xf5");
	cmpEncoded(null, "0xf6");
	
	// raw arrays, ubyte[]
	cmpEncoded(cast(ubyte[])[], "0x40");
	cmpEncoded(cast(ubyte[])[1, 2, 3, 4], "0x4401020304");
	
	// strings
	cmpEncoded("", "0x60");
	cmpEncoded("a", "0x6161");
	cmpEncoded("IETF", "0x6449455446");
	cmpEncoded("\"\\", "0x62225c");
	cmpEncoded("\u00fc", "0x62c3bc");
	cmpEncoded("\u6c34", "0x63e6b0b4");
	//cmpEncoded("\ud800\udd51", "0x64f0908591"); // invalid character

	import std.typecons : tuple;
	// arrays
	cmpEncoded([], "0x80");
	cmpEncoded([1, 2, 3], "0x83010203");
	cmpEncoded(tuple(1, [2, 3], [4, 5]), "0x8301820203820405");
	enum arr = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14,
				15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25];
	cmpEncoded(arr, "0x98190102030405060708090a0b0c0d0e0f101112131415161718181819");

	// AA
	uint[uint] emptyAA;
	cmpEncoded(emptyAA, "0xa0");
	cmpEncoded([1:2, 3:4], "0xa201020304");
	
	//cmpEncoded({"a": 1, "b": [2, 3]}, "0xa26161016162820203");
	//cmpEncoded(["a", {"b": "c"}], "0x826161a161626163");
	cmpEncoded(["a": "A", "b": "B", "c":"C", "d": "D", "e": "E"],
		"0xa56161614161626142616361436164614461656145");
	//cmpEncoded((_ h'0102', h'030405'), "0x5f42010243030405ff");
	//cmpEncoded((_ "strea", "ming"), "0x7f657374726561646d696e67ff");
	//cmpEncoded([_ ], "0x9fff");
	//cmpEncoded([_ 1, [2, 3], [_ 4, 5]], "0x9f018202039f0405ffff");
	//cmpEncoded([_ 1, [2, 3], [4, 5]], "0x9f01820203820405ff");
	//cmpEncoded([1, [2, 3], [_ 4, 5]], "0x83018202039f0405ff");
	//cmpEncoded([1, [_ 2, 3], [4, 5]], "0x83019f0203ff820405");
	//cmpEncoded([_ 1, 2, 3, 4, 5, 6, 7, 8,	9, 10, 11, 12, 13, 14, 15,
	//	16, 17, 18, 19, 20, 21, 22, 23, 24, 25],
	//	"0x9f0102030405060708090a0b0c0d0e0f101112131415161718181819ff");
	//cmpEncoded({_ "a": 1, "b": [_ 2, 3]}, "0xbf61610161629f0203ffff");
	//cmpEncoded(["a", {_ "b": "c"}], "0x826161bf61626163ff");
	//cmpEncoded({_ "Fun": true, "Amt": -2}, "0xbf6346756ef563416d7421ff");

	cmpEncoded(cast(ubyte)42, "0x182a");
	cmpEncoded(cast(ushort)42, "0x182a");
	cmpEncoded(cast(uint)42, "0x182a");
	cmpEncoded(cast(ulong)42, "0x182a");
	cmpEncoded(cast(byte)42, "0x182a");
	cmpEncoded(cast(short)42, "0x182a");
	cmpEncoded(cast(int)42, "0x182a");
	cmpEncoded(cast(long)42, "0x182a");
}

unittest // decoding decodeCbor
{
	import std.range : only, chain, iota;
	import std.exception;
	import std.bitmanip;
	import std.array;
	import std.algorithm : equal;

	alias ntob = nativeToBigEndian;
	alias bton = bigEndianToNative;

	// Integer 0x00..0x17 (0..23)
	foreach(ubyte item; 0x00..0x18)
	{
		assert(decodeCbor(only(item))==item);
	}

	// Unsigned integer (one-byte uint8_t follows)
	assert(decodeCbor(cast(ubyte[])[0x18, ubyte.max]) == ubyte.max);
	assertThrown!CborException(decodeCbor(cast(ubyte[])[0x18]));

	// Unsigned integer (two-byte uint16_t follows)
	assert(decodeCbor(chain(cast(ubyte[])[0x19], ntob!ushort(1234)[])) == 1234);
	assertThrown!CborException(decodeCbor(cast(ubyte[])[0x19]));

	// Unsigned integer (four-byte uint_t follows)
	assert(decodeCbor(chain(cast(ubyte[])[0x1a], ntob!uint(1234)[])) == 1234);
	assertThrown!CborException(decodeCbor(cast(ubyte[])[0x1a]));

	// Unsigned integer (eight-byte uint64_t follows)
	assert(decodeCbor(chain(cast(ubyte[])[0x1b], ntob!ulong(1234)[])) == 1234);
	assertThrown!CborException(decodeCbor(cast(ubyte[])[0x1b]));

	// Negative integer -1-0x00..-1-0x17 (-1..-24)
	foreach(ubyte item; 0x20..0x38) // [-1..-24]
	{
		assert(decodeCbor(only(item)) == -cast(long)item+0x1f);
	}

	// Negative integer -1-n (one-byte uint8_t for n follows)
	assert(decodeCbor(cast(ubyte[])[0x38, byte.max-1]) == -byte.max);
	assertThrown!CborException(decodeCbor(cast(ubyte[])[0x38]));

	// Negative integer -1-n (two-byte uint16_t for n follows)
	assert(decodeCbor(chain(cast(ubyte[])[0x39], ntob!short(1234-1)[])) == -1234);
	assertThrown!CborException(decodeCbor(cast(ubyte[])[0x39]));

	// Negative integer -1-n (four-byte uint_t for n follows)
	assert(decodeCbor(chain(cast(ubyte[])[0x3a], ntob!int(1234-1)[])) == -1234);
	assertThrown!CborException(decodeCbor(cast(ubyte[])[0x3a]));

	// Negative integer -1-n (eight-byte uint64_t for n follows)
	assert(decodeCbor(chain(cast(ubyte[])[0x3b], ntob!long(1234-1)[])) == -1234);
	assertThrown!CborException(decodeCbor(cast(ubyte[])[0x3b]));

	// byte string (0x00..0x17 bytes follow)
	foreach(ubyte item; 0x40..0x58)
	{
		assert(equal(
			decodeCbor(chain(
				cast(ubyte[])[item],
				iota(cast(ubyte)(item - 0x40)))
			).via.raw,
			iota(0, item - 0x40)
			));
	}

	// byte string (one-byte uint8_t for n, and then n bytes follow)
	assert(decodeCbor(cast(ubyte[])[0x58, 1, 42]) == cast(ubyte[])[42]);
	// byte string (two-byte uint16_t for n, and then n bytes follow)
	assert(decodeCbor(cast(ubyte[])[0x59, 0, 1, 42]) == cast(ubyte[])[42]);
	// byte string (four-byte uint_t for n, and then n bytes follow)
	assert(decodeCbor(cast(ubyte[])[0x5a, 0, 0, 0, 1, 42]) == cast(ubyte[])[42]);
	// byte string (eight-byte uint64_t for n, and then n bytes follow)
	assert(decodeCbor(cast(ubyte[])[0x5b, 0, 0, 0, 0, 0, 0, 0, 1, 42]) == cast(ubyte[])[42]);

	// UTF-8 string (0x00..0x17 bytes follow)
	assert(decodeCbor(cast(ubyte[])[0x64] ~ cast(ubyte[])"abcd") == "abcd");
	// UTF-8 string (one-byte uint8_t for n, and then n bytes follow)
	assert(decodeCbor(cast(ubyte[])[0x78, 4] ~ cast(ubyte[])"abcd") == "abcd");
	// UTF-8 string (two-byte uint16_t for n, and then n bytes follow)
	assert(decodeCbor(cast(ubyte[])[0x79, 0, 4] ~ cast(ubyte[])"abcd") == "abcd");
	// UTF-8 string (four-byte uint_t for n, and then n bytes follow)
	assert(decodeCbor(cast(ubyte[])[0x7a, 0, 0, 0, 4] ~ cast(ubyte[])"abcd") == "abcd");
	// UTF-8 string (eight-byte uint64_t for n, and then n bytes follow)
	assert(decodeCbor(cast(ubyte[])[0x7b, 0, 0, 0, 0, 0, 0, 0, 4] ~ cast(ubyte[])"abcd") == "abcd");
	// UTF-8 string, UTF-8 strings follow, terminated by "break"

	// array (0x00..0x17 data items follow)
	assert(decodeCbor(cast(ubyte[])[0x84, 1, 2, 3, 4]) == [1, 2, 3, 4]);
	// array (one-byte uint8_t for n, and then n data items follow)
	assert(decodeCbor(cast(ubyte[])[0x98, 4, 1, 2, 3, 4]) == [1, 2, 3, 4]);
	// array (two-byte uint16_t for n, and then n data items follow)
	assert(decodeCbor(cast(ubyte[])[0x99, 0, 4, 1, 2, 3, 4]) == [1, 2, 3, 4]);
	// array (four-byte uint_t for n, and then n data items follow)
	assert(decodeCbor(cast(ubyte[])[0x9a, 0, 0, 0, 4, 1, 2, 3, 4]) == [1, 2, 3, 4]);
	// array (eight-byte uint64_t for n, and then n data items follow)
	assert(decodeCbor(cast(ubyte[])[0x9b, 0, 0, 0, 0, 0, 0, 0, 4, 1, 2, 3, 4]) == [1, 2, 3, 4]);
	// array, data items follow, terminated by "break"
	
	// map (0x00..0x17 pairs of data items follow)
	assert(decodeCbor(getEncoded([1:2, 3:4])) == [1:2, 3:4]);
	assert(decodeCbor(cast(ubyte[])[0xa2, 1, 2, 3, 4]) == [1:2, 3:4]);
	// map (one-byte uint8_t for n, and then n pairs of data items follow)   
	assert(decodeCbor(cast(ubyte[])[0xb8, 2, 1, 2, 3, 4]) == [1:2, 3:4]);
	// map (two-byte uint16_t for n, and then n pairs of data items follow)
	assert(decodeCbor(cast(ubyte[])[0xb9, 0, 2, 1, 2, 3, 4]) == [1:2, 3:4]);
	// map (four-byte uint_t for n, and then n pairs of data items follow)
	assert(decodeCbor(cast(ubyte[])[0xba, 0, 0, 0, 2, 1, 2, 3, 4]) == [1:2, 3:4]);
	// map (eight-byte uint64_t for n, and then n pairs of data items follow)
	assert(decodeCbor(cast(ubyte[])[0xbb, 0, 0, 0, 0, 0, 0, 0, 2, 1, 2, 3, 4]) == [1:2, 3:4]);

	// False
	assert(decodeCbor(cast(ubyte[])[0xf4]) == false);
	// True
	assert(decodeCbor(cast(ubyte[])[0xf5]) == true);
	// Null
	assert(decodeCbor(cast(ubyte[])[0xf6]) == CborValue(null));
	// Undefined
	assert(decodeCbor(cast(ubyte[])[0xf7]) == CborValue(CborValue.Type.undefined));
	
	// (simple value, one byte follows) 0xf8

	// Half-Precision Float (two-byte IEEE 754)
	__HalfRep hr = {h : CustomFloat!16(1.234f)};
	assert(decodeCbor(cast(ubyte[])[0xf9] ~
		ntob!ushort((hr.u))[]) == CborValue(CustomFloat!16(1.234f).get!float));

	// Single-Precision Float (four-byte IEEE 754)
	assert(decodeCbor(cast(ubyte[])[0xfa] ~
		ntob!uint((cast(__FloatRep)1.234f).u)[]) == CborValue(1.234f));
	
	// Double-Precision Float (eight-byte IEEE 754)
	assert(decodeCbor(cast(ubyte[])[0xfb] ~
		ntob!ulong((cast(__DoubleRep)1.234).u)[]) == CborValue(1.234));	
}

unittest // test tag ignoring
{
	assert(decodeCbor(cast(ubyte[])[0xc0, 0x18, ubyte.max]) == ubyte.max);

	assert(decodeCbor(cast(ubyte[])[0xd8, 0, 0x18, ubyte.max]) == ubyte.max);
	assert(decodeCbor(cast(ubyte[])[0xd9, 0, 0, 0x18, ubyte.max]) == ubyte.max);
	assert(decodeCbor(cast(ubyte[])[0xda, 0, 0, 0, 0, 0x18, ubyte.max]) == ubyte.max);
	assert(decodeCbor(cast(ubyte[])[0xdb, 0, 0, 0, 0, 0, 0, 0, 0, 0x18, ubyte.max]) == ubyte.max);
}

///
unittest // decoding exact
{
	static struct Inner
	{
		int[] array;
		string someText;
	}

	static struct Test1
	{
		ubyte b;
		short s;
		uint i;
		long l;
		float f;
		double d;
		ubyte[] arr;
		string str;
		Inner inner;

		void fun(){} // not encoded
		void* pointer; // not encoded
	}

	ubyte[1024] buf1;
	size_t size;

	Test1 test = Test1(42, -120, 111111, -123456789, 0.1234, -0.987654,
		cast(ubyte[])[1,2,3,4,5,6,7,8], "It is a test string",
		Inner([1,2,3,4,5], "Test of inner struct"));

	size = encodeCborArray(buf1[], test);
	Test1 result = decodeCborSingle!Test1(buf1[0..size]);
	assert(test == result);

	import std.typecons : Tuple;

	alias TupleVal = Tuple!(int, string, byte, string);
	auto testTuple = TupleVal(1, "hello", 56, "there");

	size = encodeCborArray(buf1[], testTuple);
	TupleVal resultTuple = decodeCborSingle!TupleVal(buf1[0..size]);
	assert(testTuple == resultTuple);

	static class Inner2
	{
		int val;
		ulong u;
	}

	static class Test2
	{
		ubyte b;
		short s;
		uint i;
		long l;
		float f;
		double d;
		ubyte[] arr;
		string str;
		Inner2 inner;
	}

	Test2 testClass = new Test2();
	testClass.b = 42;
	testClass.s = -120;
	testClass.i = 111111;
	testClass.l = -123456789;
	testClass.f = 0.1234;
	testClass.d = -0.987654;
	testClass.arr = cast(ubyte[])[1,2,3,4,5,6,7,8];
	testClass.str = "It is a test string";
	testClass.inner = null;
	
	size = encodeCborArray(buf1[], testClass);
	ubyte[] encodedBuf = buf1[0..size];
	Test2 resultClass = decodeCborSingle!Test2(encodedBuf);
	assert(encodedBuf.length == 0);

	foreach(i, m; resultClass.tupleof)
		assert(testClass.tupleof[i] == m);

	testClass.inner = new Inner2;
	testClass.inner.val = -555;
	testClass.inner.u = 123456789;

	size = encodeCborArray(buf1[], testClass);
	resultClass = decodeCborSingle!Test2(buf1[0..size]);

	foreach(i, m; resultClass.inner.tupleof)
		assert(testClass.inner.tupleof[i] == m);
}

unittest // decoding with dup
{
	ubyte[128] buf1;
	size_t size;

	// with dup
	size = encodeCbor(buf1[], cast(ubyte[])[0, 1, 2, 3, 4, 5]);
	ubyte[] data = decodeCborSingleDup!(ubyte[])(buf1[0..size]);
	buf1[] = 0;

	assert(data == [0, 1, 2, 3, 4, 5]);

	// without dup
	size = encodeCbor(buf1[], cast(ubyte[])[0, 1, 2, 3, 4, 5]);
	data = decodeCborSingle!(ubyte[])(buf1[0..size]);
	buf1[] = 0;

	assert(data == [0, 0, 0, 0, 0, 0]);

	// dup is only needed for ubyte[] and string types,
	// because they can be sliced from ubyte[] input range
	size = encodeCbor(buf1[], [0, 1, 2, 3, 4, 5]); // int array
	int[] intData = decodeCborSingle!(int[])(buf1[0..size]); // no dup
	buf1[] = 0;

	assert(intData == [0, 1, 2, 3, 4, 5]);

	// also no slicing will occur if data was encoded as regular array and than
	// ubyte[] retreived. integer[] -> ubyte[] cast would occur causing possible data loss.
	// size = encodeCbor(buf1[], [0, 1, 2, 3, 4, 5]); // int array
	// integer array cannot be decoded as ubyte[], only raw arrays can
	// data = decodeCborSingle!(ubyte[])(buf1[0..size]); // CborException: Attempt to cast array to ubyte[]
}

unittest // static arrays
{
	ubyte[128] buf;
	size_t size;

	// raw static array
	size = encodeCbor(buf[], cast(ubyte[6])[0, 1, 2, 3, 4, 5]);
	ubyte[6] data1 = decodeCborSingle!(ubyte[6])(buf[0..size]);
	assert(data1 == [0, 1, 2, 3, 4, 5]);

	// regular static array
	size = encodeCbor(buf[], cast(int[6])[0, 1, 2, 3, 4, 5]);
	int[6] data2 = decodeCborSingle!(int[6])(buf[0..size]);
	assert(data2 == [0, 1, 2, 3, 4, 5]);
}

unittest // const
{
	ubyte[1024] buf;
	size_t size = encodeCbor(buf[], cast(const)0.0);

	double cam2 = decodeCborSingle!double(buf[0..size]);
}

unittest // using output range
{
	import std.array : Appender;

	Appender!(ubyte[]) buffer;
	ubyte[] testData = [0, 1, 2, 3, 4, 5];

	size_t size = encodeCbor(buffer, testData);

	assert(testData == decodeCborSingle!(ubyte[])(buffer.data));
}

unittest // recursive type
{
	import std.array : Appender;

	Appender!(ubyte[]) a;
	class Class
	{
		Class[] groups;
	}
	encodeCborArray(a, Class[].init);
}

unittest // char arrays
{
	ubyte[1024] buf;

	size_t size = encodeCbor(buf[], cast(char[])"abc");
	char[] str1 = decodeCborSingle!(char[])(buf[0..size]);
	assert(str1 == "abc");

	size = encodeCbor(buf[], cast(const char[])"abc");
	const char[] str2 = decodeCborSingle!(const char[])(buf[0..size]);
	assert(str2 == "abc");

	size = encodeCbor(buf[], cast(immutable char[])"abc");
	immutable char[] str3 = decodeCborSingle!(immutable char[])(buf[0..size]);
	assert(str3 == "abc");
}

unittest // char wchar dchar
{
	ubyte[1024] buf;
	char testChar = 'c';

	size_t size = encodeCbor(buf[], cast(char)testChar);
	char chr = decodeCborSingle!(char)(buf[0..size]);
	assert(chr == testChar);

	size = encodeCbor(buf[], cast(wchar)testChar);
	wchar wchr = decodeCborSingle!(wchar)(buf[0..size]);
	assert(wchr == testChar);

	size = encodeCbor(buf[], cast(dchar)testChar);
	dchar dchr = decodeCborSingle!(dchar)(buf[0..size]);
	assert(dchr == testChar);

	size = encodeCbor(buf[], cast(const char)testChar);
	const char constchr = decodeCborSingle!(const char)(buf[0..size]);
	assert(constchr == testChar);

	size = encodeCbor(buf[], cast(const wchar)testChar);
	const wchar constwchr = decodeCborSingle!(const wchar)(buf[0..size]);
	assert(constwchr == testChar);

	size = encodeCbor(buf[], cast(immutable dchar)testChar);
	immutable dchar immdchr = decodeCborSingle!(immutable dchar)(buf[0..size]);
	assert(immdchr == testChar);
}

unittest // wstring dstring; static char wchar dchar arrays
{
	ubyte[1024] buf;

	size_t size = encodeCbor(buf[], "hello"w);
	wstring wstr = decodeCborSingle!(wstring)(buf[0..size]);
	assert(wstr == "hello"w);

	size = encodeCbor(buf[], "hello"d);
	dstring dstr = decodeCborSingle!(dstring)(buf[0..size]);
	assert(dstr == "hello"d);

	size = encodeCbor(buf[], cast(char[5])"hello");
	char[5] str1 = decodeCborSingle!(char[5])(buf[0..size]);
	assert(str1 == "hello");

	size = encodeCbor(buf[], cast(wchar[5])"hello");
	wchar[5] wstr1 = decodeCborSingle!(wchar[5])(buf[0..size]);
	assert(wstr1 == "hello"w);

	size = encodeCbor(buf[], cast(dchar[5])"hello");
	dchar[5] dstr1 = decodeCborSingle!(dchar[5])(buf[0..size]);
	assert(dstr1 == "hello"d);
}

unittest // char[] wchar[] dchar[]
{
	ubyte[1024] buf;

	size_t size = encodeCbor(buf[], cast(char[])"hello");
	char[] str1 = decodeCborSingle!(char[])(buf[0..size]);
	assert(str1 == "hello");

	size = encodeCbor(buf[], cast(wchar[])"hello");
	wchar[] wstr1 = decodeCborSingle!(wchar[])(buf[0..size]);
	assert(wstr1 == "hello"w);

	size = encodeCbor(buf[], cast(dchar[])"hello");
	dchar[] dstr1 = decodeCborSingle!(dchar[])(buf[0..size]);
	assert(dstr1 == "hello"d);
}

private:

@safe pure
void onCastErrorToFrom(To)(CborValue.Type from, string file = __FILE__, size_t line = __LINE__)
{
	throw new CborException(format("Attempt to cast %s to %s", from, typeid(To)), file, line);
}

@safe pure
void onInsufficientInput(string file = __FILE__, size_t line = __LINE__)
{
	throw new CborException("Input range is too short", file, line);
}

@safe pure
void onUnsupportedTag(ubyte tag, string file = __FILE__, size_t line = __LINE__)
{
	throw new CborException(format("Unsupported tag found: %02x", tag), file, line);
}