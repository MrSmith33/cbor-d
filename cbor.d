/**
Copyright: Copyright (c) 2014-2016 Andrey Penechko.
License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors: Andrey Penechko.

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
private import std.traits : Unqual, isArray, isAssociativeArray, isBoolean, isDynamicArray,
	isExpressionTuple, isFloatingPoint, isIntegral, isSomeChar, isStaticArray, isUnsigned;
private import std.typecons : Flag, Yes, No;
private import std.range : ElementEncodingType, hasLength, save;
private import std.conv : to;
private import std.utf : byChar;

//------------------------------------------------------------------------------
//		EEEEEEE NN   NN  CCCCC   OOOOO  DDDDD   IIIII NN   NN   GGGG  
//		EE      NNN  NN CC    C OO   OO DD  DD   III  NNN  NN  GG  GG 
//		EEEEE   NN N NN CC      OO   OO DD   DD  III  NN N NN GG      
//		EE      NN  NNN CC    C OO   OO DD   DD  III  NN  NNN GG   GG 
//		EEEEEEE NN   NN  CCCCC   OOOO0  DDDDDD  IIIII NN   NN  GGGGGG 
//------------------------------------------------------------------------------

private import std.range : isInputRange, isOutputRange, ElementType;
private import std.typecons : isTuple;

//------------------------------------------------------------------------------
// Simple types (int, float, bool, null, undefined, break, simple)
//------------------------------------------------------------------------------

// Encode integer types as separate type or as part of arrays or map.
private size_t encodeLongType(R)(auto ref R sink, ubyte majorType, ulong length)
	if(isOutputRange!(R, ubyte))
{
	import std.bitmanip : nativeToBigEndian;

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
size_t encodeCborNull(R)(auto ref R sink)
	if(isOutputRange!(R, ubyte))
{
	putChecked(sink, cast(ubyte)0xf6);
	return 1;
}

/// Encodes undefined.
size_t encodeCborUndefined(R)(auto ref R sink) if(isOutputRange!(R, ubyte))
{
	putChecked(sink, cast(ubyte)0xf7);
	return 1;
}

/// Encodes break. Use after all items of indefinite-length sequence were encoded.
size_t encodeCborBreak(R)(auto ref R sink) if(isOutputRange!(R, ubyte))
{
	putChecked(sink, cast(ubyte)0xff);
	return 1;
}

/// Encodes simple. Simple data type is essentially a number that has special meaning.
/// Value must lie in range [0..23] or [32..255].
size_t encodeCborSimple(R)(auto ref R sink, ubyte simple) if(isOutputRange!(R, ubyte))
{
	enum ubyte majorType = 7 << 5;
	if (simple < 24) {
		putChecked(sink, cast(ubyte)(majorType | simple));
		return 1;
	} else if (simple > 31) {
		putChecked(sink, cast(ubyte)0xf8);
		putChecked(sink, simple);
		return 2;
	} else {
		assert(false, format("Invalid simple value %s", simple));
	}
}

//------------------------------------------------------------------------------
// Complex data (byte-string, text-string, array, map)
//------------------------------------------------------------------------------
// Headers
//------------------------------------------------------------------------------

size_t encodeCborBytesHeader(R)(auto ref R sink, ulong bytesLength) if(isOutputRange!(R, ubyte))
{
	return encodeLongType(sink, 2, bytesLength);
}

size_t encodeCborBytesHeader(R)(auto ref R sink) if(isOutputRange!(R, ubyte))
{
	putChecked(sink, cast(ubyte)0x5f);
	return 1;
}

size_t encodeCborStringHeader(R)(auto ref R sink, ulong textLength) if(isOutputRange!(R, ubyte))
{
	return encodeLongType(sink, 3, textLength);
}

size_t encodeCborStringHeader(R)(auto ref R sink) if(isOutputRange!(R, ubyte))
{
	putChecked(sink, cast(ubyte)0x7f);
	return 1;
}

size_t encodeCborArrayHeader(R)(auto ref R sink, ulong arrayLength) if(isOutputRange!(R, ubyte))
{
	return encodeLongType(sink, 4, arrayLength);
}

size_t encodeCborArrayHeader(R)(auto ref R sink) if(isOutputRange!(R, ubyte))
{
	putChecked(sink, cast(ubyte)0x9f);
	return 1;
}

size_t encodeCborMapHeader(R)(auto ref R sink, ulong mapLength) if(isOutputRange!(R, ubyte))
{
	return encodeLongType(sink, 5, mapLength);
}

size_t encodeCborMapHeader(R)(auto ref R sink) if(isOutputRange!(R, ubyte))
{
	putChecked(sink, cast(ubyte)0xbf);
	return 1;
}

//------------------------------------------------------------------------------
// Items
//------------------------------------------------------------------------------

/// Writes range of ubyte to the sink. Needs to go after a call to encodeCborBytesHeader.
/// The length of supplied range must be equal to one provided to encodeCborBytesHeader.
size_t encodeCborBytesItems(R, D)(auto ref R sink, D bytes)
	if(isOutputRange!(R, ubyte) &&
		(isArray!D || isInputRange!D) && is(Unqual!(ElementType!D) == ubyte))
{
	static if (hasLength!D) {
		static if (isStaticArray!D)
			putChecked(sink, bytes[]);
		else
			putChecked(sink, bytes);
		return bytes.length;
	} else {
		size_t count;
		putChecked(sink, bytes.tee!(a => ++count));
		return count;
	}
}

//------------------------------------------------------------------------------
// Tag
//------------------------------------------------------------------------------

///
size_t encodeCborTag(R)(auto ref R sink, ulong value) if(isOutputRange!(R, ubyte))
{
	return encodeLongType(sink, 6, value);
}

//------------------------------------------------------------------------------
//	 SSSSS  EEEEEEE RRRRRR  IIIII   AAA   LL      IIIII ZZZZZ   AAA   TTTTTTT IIIII  OOOOO  NN   NN 
//	SS      EE      RR   RR  III   AAAAA  LL       III     ZZ  AAAAA    TTT    III  OO   OO NNN  NN 
//	 SSSSS  EEEEE   RRRRRR   III  AA   AA LL       III    ZZ  AA   AA   TTT    III  OO   OO NN N NN 
//	     SS EE      RR  RR   III  AAAAAAA LL       III   ZZ   AAAAAAA   TTT    III  OO   OO NN  NNN 
//	 SSSSS  EEEEEEE RR   RR IIIII AA   AA LLLLLLL IIIII ZZZZZ AA   AA   TTT   IIIII  OOOO0  NN   NN 
//	                                                                                                
//------------------------------------------------------------------------------

/// Encodes value E into output range sink.
/// Returns number of bytes written to sink.
/// If flatten flag is yes then static arrays and structs will be encoded in place without headers.
size_t encodeCbor(Flag!"Flatten" flatten = No.Flatten, R, E)(auto ref R sink, auto ref const E value)
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
		return encodeCborNull(sink);
	}
	else static if ((isArray!E || isInputRange!E) && is(Unqual!(ElementType!E) == ubyte))
	{
		return encodeCborBytes!(flatten)(sink, value);
	}
	else static if ((isArray!E || isInputRange!E) && isSomeChar!(Unqual!(ElementEncodingType!E)))
	{
		return encodeCborString!(flatten)(sink, value);
	}
	else static if (isInputRange!E || isArray!E || isTuple!E ||
		is(E == class) || is(E == struct))
	{
		return encodeCborArray!(flatten)(sink, value);
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

/// Encodes range of ubytes.
size_t encodeCborBytes(Flag!"Flatten" flatten = No.Flatten, R, E)(auto ref R sink, E value)
	if(isOutputRange!(R, ubyte) &&
		(isArray!E || isInputRange!E) && is(Unqual!(ElementType!E) == ubyte))
{
	size_t size = 0;
	// drop type and length for static arrays in flatten mode
	static if (!needsFlattening!(E, flatten))
	{
		size = encodeCborBytesHeader(sink, value.length);
	}
	size += encodeCborBytesItems(sink, value);
	return size;
}

/// Encodes string.
size_t encodeCborString(Flag!"Flatten" flatten = No.Flatten, R, E)(auto ref R sink, E value)
	if(isOutputRange!(R, ubyte) && isSomeChar!(Unqual!(ElementEncodingType!E)))
{
	size_t size = 0;
	// drop type and length for static arrays in flatten mode
	static if (!needsFlattening!(E, flatten))
	{
		size = encodeCborStringHeader(sink, value.length);
	}
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
size_t encodeCborArray(Flag!"Flatten" flatten = No.Flatten, R, E)(auto ref R sink, E value)
	if(isOutputRange!(R, ubyte) &&
	(isInputRange!E || isArray!E || isTuple!E))
{
	static if (isArray!E && is(Unqual!(ElementType!E) == void)) // accept []
	{
		return encodeCborArrayHeader(sink, 0);
	}
	else
	{
		size_t size = 0;
		// drop type and length for static arrays and expression tuples in flatten mode
		static if (!needsFlattening!(E, flatten))
			size = encodeCborArrayHeader(sink, value.length);

		foreach(item; value)
			size += encodeCbor!(flatten)(sink, item);
		return size;
	}
}

/// Encodes structs and classes as cbor array.
size_t encodeCborArray(Flag!"Flatten" flatten = No.Flatten, R, A)(auto ref R sink, A aggregate)
	if(isOutputRange!(R, ubyte) &&
		(is(A == struct) || is(A == class)) &&
		!isTuple!A)
{
	return encodeCborAggregate!(Flag!"WithFieldName".no, flatten)(sink, aggregate);
}

/// Encodes asociative array as cbor map.
size_t encodeCborMap(R, E)(auto ref R sink, E value)
	if(isOutputRange!(R, ubyte) && isAssociativeArray!E)
{
	size_t size = encodeCborMapHeader(sink, value.length);
	foreach(key, element; value)
	{
		size += encodeCbor(sink, key);
		size += encodeCbor(sink, element);
	}
	return size;
}

/// Encodes structs and classes as cbor map.
/// Note, that decoding of structs and classes from maps is not supported (yet).
size_t encodeCborMap(Flag!"Flatten" flatten = Flag!"Flatten".no, R, A)(auto ref R sink, A aggregate)
	if(isOutputRange!(R, ubyte) && (is(A == struct) || is(A == class)) && !isTuple!A)
{
	return encodeCborAggregate!(Flag!"WithFieldName".yes, flatten)(sink, aggregate);
}

/// Encodes classes and structs. If withFieldName is yes, than value is encoded as map.
/// If withFieldName is no, then value is encoded as an array.
/// If flatten flag is yes then static arrays and structs will be encoded in place without headers.
size_t encodeCborAggregate(
	Flag!"WithFieldName" withFieldName,
	Flag!"Flatten" flatten = Flag!"Flatten".no,
	R,
	A)(
	auto ref R sink,
	auto ref A aggregate)
	if (isOutputRange!(R, ubyte) && (is(A == struct) || is(A == class)))
{
	size_t size;
	static if (is(A == class))
		if (aggregate is null)
			return encodeCbor(sink, null);

	// flatten structs only
	static if (!needsFlattening!(A, flatten))
	{
		static if (withFieldName)
			size += encodeCborMapHeader(sink, numEncodableMembers!A);
		else
			size += encodeCborArrayHeader(sink, numEncodableMembers!A);
	}

	foreach(i, member; aggregate.tupleof)
	{
		static if (isEncodedField!(typeof(member)))
		{
			// when encoded as map order is unspecified and flatten is not possible
			static if (withFieldName)
			{
				size += encodeCborString(sink, __traits(identifier, aggregate.tupleof[i]));
				size += encodeCbor(sink, member);
			}
			else
				size += encodeCbor!flatten(sink, member);
		}
	}
	return size;
}

//------------------------------------------------------------------------------
//		 SSSSS  TTTTTTT  OOOOO  RRRRRR    AAA     GGGG  EEEEEEE 
//		SS        TTT   OO   OO RR   RR  AAAAA   GG  GG EE      
//		 SSSSS    TTT   OO   OO RRRRRR  AA   AA GG      EEEEE   
//		     SS   TTT   OO   OO RR  RR  AAAAAAA GG   GG EE      
//		 SSSSS    TTT    OOOO0  RR   RR AA   AA  GGGGGG EEEEEEE 
//------------------------------------------------------------------------------

///
enum CborTokenType : ubyte
{
	/// CborToken.uinteger stores array length.
	/// Indefinite-length array has no length.
	/// Header is followed by zero or more data items, terminated by break.
	arrayHeader = 0b000,
	arrayIndefiniteHeader = 0b001, /// ditto

	/// CborToken.uinteger stores map's pair count.
	/// Indefinite-length map has no length.
	/// Header is followed by zero or more pairs, terminated by break.
	mapHeader = 0b010,
	mapIndefiniteHeader = 0b011, /// ditto
	
	/// CborToken.uinteger stores byte string length.
	/// Indefinite-length byte string has no length.
	/// Header is follower by zero or more definite-length byte strings terminated by break.
	bytesHeader = 0b100,
	bytesIndefiniteHeader = 0b101, /// ditto

	/// CborToken.uinteger stores text string length.
	/// Indefinite-length text string has no length.
	/// Header is follower by zero or more definite-length text strings terminated by break.
	textHeader = 0b110,
	textIndefiniteHeader = 0b111, /// ditto

	undefined, /// CborToken value is undefined.
	nil, /// CborToken value is null.
	boolean, /// CborToken.boolean stores actual value.
	tag, /// uinteger stores tag value.
	simple, /// Simple value is integer in range [0-255], stored in uinteger.
	breakCode, /// "break" stop code, used to terminate indefinite-length sequences.

	posinteger, /// CborToken.uinteger stores actual value.
	neginteger, /// CborToken.integer stores actual value.
	floating, /// CborToken.floating stores actual value.
}

///
struct CborToken
{
	CborTokenType type;
	union
	{
		bool boolean;
		long integer;
		// used for storing positive integers, array, map, raw and text size
		double floating;
		ulong uinteger;
	}

	this(CborTokenType t) {
		type = t;
	}
	this(CborTokenType t, bool v) {
		type = t;
		boolean = v;
	}
	this(CborTokenType t, long v) {
		type = t;
		integer = v;
	}
	this(CborTokenType t, double v) {
		type = t;
		floating = v;
	}

	bool opEquals(const CborToken other) {
		ubyte[] thisRep = (cast(ubyte*)(&this))[0..this.sizeof];
		ubyte[] otherRep = (cast(ubyte*)(&other))[0..other.sizeof];
		return thisRep == otherRep;
	}

	string toString()
	{
		import std.string : format;
		final switch(type) with(CborTokenType)
		{
			case boolean: return format("CborToken(%s)", boolean);
			case nil: return "CborToken(null)";
			case undefined: return "CborToken(undefined)";
			case tag: return format("CborToken(tag, %s)", type, uinteger);
			case simple: return format("CborToken(simple, %s)", uinteger);
			case breakCode: return format("CborToken(break)");
			case posinteger: return format("CborToken(%s, %s)", type, uinteger);
			case neginteger: return format("CborToken(%s, %s)", type, integer);
			case floating: return format("CborToken(%s, %s)", type, floating);
			case arrayHeader: return format("CborToken(array, %s)", uinteger);
			case arrayIndefiniteHeader: return format("CborToken(arrayIndefiniteLength, _)");
			case mapHeader: return format("CborToken(map, %s)", uinteger);
			case mapIndefiniteHeader: return format("CborToken(mapIndefiniteLength, _)");
			case bytesHeader: return format("CborToken(byteString, %s)", uinteger);
			case bytesIndefiniteHeader: return format("CborToken(byteStringIndefiniteLength, _)");
			case textHeader: return format("CborToken(textString, %s)", uinteger);
			case textIndefiniteHeader: return format("CborToken(textStringIndefiniteLength, _)");
		}
	}
}

//------------------------------------------------------------------------------
//		DDDDD   EEEEEEE  CCCCC   OOOOO  DDDDD   IIIII NN   NN   GGGG  
//		DD  DD  EE      CC    C OO   OO DD  DD   III  NNN  NN  GG  GG 
//		DD   DD EEEEE   CC      OO   OO DD   DD  III  NN N NN GG      
//		DD   DD EE      CC    C OO   OO DD   DD  III  NN  NNN GG   GG 
//		DDDDDD  EEEEEEE  CCCCC   OOOO0  DDDDDD  IIIII NN   NN  GGGGGG 
//------------------------------------------------------------------------------

CborToken decodeCborToken(R)(auto ref R input)
	if(isInputRange!R && is(ElementType!R == ubyte))
{
	import std.array;
	import std.bitmanip;

	if (input.empty) onInsufficientInput();

	ubyte item = input.front;
	input.popFront;

	switch(item)
	{
		case 0x00: .. case 0x17: // Integer 0x00..0x17 (0..23)
			return CborToken(CborTokenType.posinteger, item);
		case 0x18: // Unsigned integer (one-byte uint8_t follows)
			return CborToken(CborTokenType.posinteger, readInteger!ubyte(input));
		case 0x19: // Unsigned integer (two-byte uint16_t follows)
			return CborToken(CborTokenType.posinteger, readInteger!ushort(input));
		case 0x1a: // Unsigned integer (four-byte uint32_t follows)
			return CborToken(CborTokenType.posinteger, readInteger!uint(input));
		case 0x1b: // Unsigned integer (eight-byte uint64_t follows)
			return CborToken(CborTokenType.posinteger, readInteger!ulong(input));
		case 0x20: .. case 0x37: // Negative integer -1-0x00..-1-0x17 (-1..-24)
			return CborToken(CborTokenType.neginteger, cast(byte)(-1 - item + 0x20));
		case 0x38: // Negative integer -1-n (one-byte uint8_t for n follows)
			return CborToken(CborTokenType.neginteger, -1 - cast(long)readInteger!ubyte(input));
		case 0x39: // Negative integer -1-n (two-byte uint16_t for n follows)
			return CborToken(CborTokenType.neginteger, -1 - cast(long)readInteger!ushort(input));
		case 0x3a: // Negative integer -1-n (four-byte uint32_t for n follows)
			return CborToken(CborTokenType.neginteger, -1 - cast(long)readInteger!uint(input));
		case 0x3b: // Negative integer -1-n (eight-byte uint64_t for n follows)
			return CborToken(CborTokenType.neginteger, -1 - cast(long)readInteger!ulong(input));
		case 0x40: .. case 0x57: // byte string (0x00..0x17 bytes follow)
			return CborToken(CborTokenType.bytesHeader, item - 0x40);
		case 0x58: // byte string (one-byte uint8_t for n, and then n bytes follow)
			return CborToken(CborTokenType.bytesHeader, readInteger!ubyte(input));
		case 0x59: // byte string (two-byte uint16_t for n, and then n bytes follow)
			return CborToken(CborTokenType.bytesHeader, readInteger!ushort(input));
		case 0x5a: // byte string (four-byte uint_t for n, and then n bytes follow)
			return CborToken(CborTokenType.bytesHeader, readInteger!uint(input));
		case 0x5b: // byte string (eight-byte uint64_t for n, and then n bytes follow)
			return CborToken(CborTokenType.bytesHeader, readInteger!ulong(input));
		case 0x5f: // indefinite-length byte string, byte strings follow, terminated by "break"
			return CborToken(CborTokenType.bytesIndefiniteHeader);
		case 0x60: .. case 0x77: // UTF-8 string (0x00..0x17 bytes follow)
			return CborToken(CborTokenType.textHeader, item - 0x60);
		case 0x78: // UTF-8 string (one-byte uint8_t for n, and then n bytes follow)
			return CborToken(CborTokenType.textHeader, readInteger!ubyte(input));
		case 0x79: // UTF-8 string (two-byte uint16_t for n, and then n bytes follow)
			return CborToken(CborTokenType.textHeader, readInteger!ushort(input));
		case 0x7a: // UTF-8 string (four-byte uint_t for n, and then n bytes follow)
			return CborToken(CborTokenType.textHeader, readInteger!uint(input));
		case 0x7b: // UTF-8 string (eight-byte uint64_t for n, and then n bytes follow)
			return CborToken(CborTokenType.textHeader, readInteger!ulong(input));
		case 0x7f: // indefinite-length UTF-8 string, UTF-8 strings follow, terminated by "break"
			return CborToken(CborTokenType.textIndefiniteHeader);
		case 0x80: .. case 0x97: // array (0x00..0x17 data items follow)
			return CborToken(CborTokenType.arrayHeader, item - 0x80);
		case 0x98: // array (one-byte uint8_t for n, and then n data items follow)
			return CborToken(CborTokenType.arrayHeader, readInteger!ubyte(input));
		case 0x99: // array (two-byte uint16_t for n, and then n data items follow)
			return CborToken(CborTokenType.arrayHeader, readInteger!ushort(input));
		case 0x9a: // array (four-byte uint_t for n, and then n data items follow)
			return CborToken(CborTokenType.arrayHeader, readInteger!uint(input));
		case 0x9b: // array (eight-byte uint64_t for n, and then n data items follow)
			return CborToken(CborTokenType.arrayHeader, readInteger!ulong(input));
		case 0x9f: // indefinite-length array, data items follow, terminated by "break"
			return CborToken(CborTokenType.arrayIndefiniteHeader);
		case 0xa0: .. case 0xb7: // map (0x00..0x17 pairs of data items follow)
			return CborToken(CborTokenType.mapHeader, item - 0xa0);
		case 0xb8: // map (one-byte uint8_t for n, and then n pairs of data items follow)   
			return CborToken(CborTokenType.mapHeader, readInteger!ubyte(input));
		case 0xb9: // map (two-byte uint16_t for n, and then n pairs of data items follow)
			return CborToken(CborTokenType.mapHeader, readInteger!ushort(input));
		case 0xba: // map (four-byte uint_t for n, and then n pairs of data items follow)
			return CborToken(CborTokenType.mapHeader, readInteger!uint(input));
		case 0xbb: // map (eight-byte uint64_t for n, and then n pairs of data items follow)
			return CborToken(CborTokenType.mapHeader, readInteger!ulong(input));
		case 0xbf: // indefinite-length map, pairs of data items follow, terminated by "break"
			return CborToken(CborTokenType.mapIndefiniteHeader);
		case 0xc0: .. case 0xd7: // (tagged item)
			return CborToken(CborTokenType.tag, item - 0xc0);
		case 0xd8: // ignore 1-byte tag
			return CborToken(CborTokenType.tag, readInteger!ubyte(input));
		case 0xd9: // ignore 2-byte tag
			return CborToken(CborTokenType.tag, readInteger!ushort(input));
		case 0xda: // ignore 4-byte tag
			return CborToken(CborTokenType.tag, readInteger!uint(input));
		case 0xdb: // ignore 8-byte tag
			return CborToken(CborTokenType.tag, readInteger!ulong(input));
		case 0xe0: .. case 0xf3: // (simple value)
			return CborToken(CborTokenType.simple, item - 0xe0);
		case 0xf4: // False
			return CborToken(CborTokenType.boolean, false);
		case 0xf5: // True
			return CborToken(CborTokenType.boolean, true);
		case 0xf6: // Null
			return CborToken(CborTokenType.nil);
		case 0xf7: // Undefined
			return CborToken(CborTokenType.undefined);
		case 0xf8: // (simple value, one byte follows)
			return CborToken(CborTokenType.simple, readInteger!ubyte(input));
		case 0xf9: // Half-Precision Float (two-byte IEEE 754)
			__HalfRep hr = {u : readInteger!ushort(input)};
			return CborToken(CborTokenType.floating, hr.h.get!double);
		case 0xfa: // Single-Precision Float (four-byte IEEE 754)
			__FloatRep fr = {u : readInteger!uint(input)};
			return CborToken(CborTokenType.floating, fr.f);
		case 0xfb: // Double-Precision Float (eight-byte IEEE 754)
			__DoubleRep dr = {u : readInteger!ulong(input)};
			return CborToken(CborTokenType.floating, dr.d);
		case 0xff: // "break" stop code
			return CborToken(CborTokenType.breakCode);
		default:
			onUnsupportedTag(item);
	}

	assert(false);
}

//------------------------------------------------------------------------------
//	DDDDD   EEEEEEE  SSSSS  EEEEEEE RRRRRR  IIIII   AAA   LL      IIIII ZZZZZ EEEEEEE 
//	DD  DD  EE      SS      EE      RR   RR  III   AAAAA  LL       III     ZZ EE      
//	DD   DD EEEEE    SSSSS  EEEEE   RRRRRR   III  AA   AA LL       III    ZZ  EEEEE   
//	DD   DD EE           SS EE      RR  RR   III  AAAAAAA LL       III   ZZ   EE      
//	DDDDDD  EEEEEEE  SSSSS  EEEEEEE RR   RR IIIII AA   AA LLLLLLL IIIII ZZZZZ EEEEEEE 
//	                                                                                  
//------------------------------------------------------------------------------

// If duplicate is true then dups incoming byte arrays and utf-8 string.
// If duplicate is false and input is array, then resulting byte arrays (ubyte[]) and
// utf-8 strings (char[], string) are slices of the input range.
void decodeCbor(
	Flag!"Duplicate" duplicate = Yes.Duplicate,
	Flag!"Flatten" flatten = No.Flatten,
	R,
	T)
	(auto ref R input, ref T outValue)
		if(isInputRange!R && is(ElementType!R == ubyte))
{
	import std.typecons : isTuple;
	CborToken token;

	static if (isIntegral!T) {
		token = decodeCborToken(input);
		if (token.type == CborTokenType.neginteger) {
			outValue = cast(T)token.integer;
			return;
		} else if (token.type == CborTokenType.posinteger) {
			outValue = cast(T)token.uinteger;
			return;
		}
		onCastErrorToFrom!T(token.type);
	} else static if (isSomeChar!T) {
		token = decodeCborToken(input);
		if (token.type == CborTokenType.posinteger) {
			outValue = cast(T)token.uinteger;
			return;
		}
		onCastErrorToFrom!T(token.type);
	} else static if (isFloatingPoint!T) {
		token = decodeCborToken(input);
		if (token.type == CborTokenType.floating) {
			outValue = cast(T)token.floating;
			return;
		}
		onCastErrorToFrom!T(token.type);
	} else static if (isBoolean!T) {
		token = decodeCborToken(input);
		if (token.type == CborTokenType.boolean) {
			outValue = token.boolean;
			return;
		}
		onCastErrorToFrom!T(token.type);
	} else static if ((isArray!T || isOutputRange!(T, ubyte)) && is(Unqual!(ElementType!T) == ubyte)) {
		decodeCborExactByteArray!(duplicate, flatten)(input, outValue);
		return;
	} else static if (isArray!T && isSomeChar!(Unqual!(ElementEncodingType!T))) {
		decodeString!(duplicate, flatten)(input, outValue);
		return;
	} else static if (isArray!T) {
		decodeSequence!(duplicate, flatten)(input, outValue);
		return;
	} else static if (is(T == class) || is(T == struct) || isTuple!T) {
		decodeAggregate!(duplicate, flatten)(input, outValue);
		return;
	} else static if (isAssociativeArray!T) {
		token = decodeCborToken(input);

		if (token.type == CborTokenType.nil) {
			outValue = null;
			return;
		}

		T map;
		alias K = typeof(T.init.keys[0]);
		alias V = typeof(T.init.values[0]);

		if (token.type == CborTokenType.mapHeader) {
			foreach (_; 0..token.uinteger) {
				K key;
				V value;
				decodeCbor!(duplicate, flatten)(input, key);
				decodeCbor!(duplicate, flatten)(input, value);
				map[key] = value;
			}
		} else if (token.type == CborTokenType.mapIndefiniteHeader) {
			while (true) {
				token = decodeCborToken(input.save);
				if (token.type == CborTokenType.breakCode) {
					break;
				} else {
					K key;
					V value;
					decodeCbor!(duplicate, flatten)(input, key);
					decodeCbor!(duplicate, flatten)(input, value);
					map[key] = value;
				}
			}
		}
		outValue = map;
		return;
	} else {
		static assert(false, "Unable to decode " ~ T.stringof);
	}

	assert(false);
}

private void decodeString(
	Flag!"Duplicate" duplicate = Yes.Duplicate,
	Flag!"Flatten" flatten = No.Flatten,
	R,
	T)
	(auto ref R input, ref T outValue)
		if(isInputRange!R && is(ElementType!R == ubyte))
{
	enum bool flat = needsFlattening!(T, flatten);
	static if (flat) {
		static assert(isStaticArray!T, "Only static arrays can be flat");
		ubyte[] bytes = readBytes(input, T.length);
		readStaticString(bytes, outValue);
		return;
	}

	CborToken token = decodeCborToken(input);
	bool definite = !(token.type & 0b001);

	if (definite) {
		static if (isDynamicArray!T)
		{
			ubyte[] bytes = readBytes(input, cast(size_t)token.uinteger);
			outValue = to!T(cast(char[])bytes);
			static if (is(Unqual!(ElementEncodingType!T) == ubyte) && duplicate)
				outValue = outValue.dup; // TODO allocation
			return;
		}
		else static if (isStaticArray!T)
		{
			ubyte[] bytes = readBytes(input, cast(size_t)token.uinteger);
			if (bytes.length != T.length) onCastErrorToFrom!T(token.type);
			readStaticString(bytes, outValue);
			return;
		}
	}
	else
	{
		static if (isDynamicArray!T)
		{
			alias Unqualified = Unqual!(ElementEncodingType!T)[];
			Unqualified output;
			size_t i;

			while (true) {
				token = decodeCborToken(input.save);
				if (token.type == CborTokenType.breakCode) {
					import std.range : dropExactly;
					dropExactly(input, 1);
					break;
				} else {
					decodeCborToken(input);
					if (token.type != CborTokenType.textHeader)
						throw new CborException(format("Expected textHeader but got %s", token.type));
					output.length += cast(size_t)token.uinteger - (output.length - i); // TODO allocation
					ubyte[] innerBytes = readBytes(input, token.uinteger);
					i += readStaticString(innerBytes, output[i..$]);
				}
			}
			output.length = i;
			outValue = cast(T)output;
			return;
		}
		else static if (isStaticArray!T)
			assert(false, "TODO");
	}
	assert(false, "TODO");
}

private size_t readStaticString(T)(ubyte[] bytes, auto ref T outValue)
{
	static if (is(Unqual!(ElementEncodingType!T) == char))
	{
		outValue[] = cast(ElementEncodingType!T[])(bytes[]);
		return bytes.length;
	}
	else
	{
		import std.utf : byChar, byWchar, byDchar;
		alias V = Unqual!(ElementEncodingType!T);
		
		static if (is(V == char))
			alias byElem = byChar;
		else static if (is(V == wchar))
			alias byElem = byWchar;
		else static if (is(V == dchar))
			alias byElem = byDchar;

		size_t i;
		foreach(c; byElem(cast(char[])bytes)) {
			outValue[i] = c;
			++i;
		}
		return i;
	}
}

private void decodeSequence(
	Flag!"Duplicate" duplicate = Yes.Duplicate,
	Flag!"Flatten" flatten = No.Flatten,
	R,
	T)
	(auto ref R input, ref T outValue)
		if(isInputRange!R && is(ElementType!R == ubyte))
{
	enum bool flat = needsFlattening!(T, flatten);
	enum bool dynamicArray = isDynamicArray!T;
	enum bool staticArray = isStaticArray!T;
	enum bool range = !(staticArray || dynamicArray);

	//------------------------------------------------------------------------------
	// Flat. Without header. Read only elements.
	static if (flat)
	{
		static assert(isStaticArray!T, "Only static arrays can be flat");
		foreach (ref elem; outValue)
			decodeCbor!(duplicate, flatten)(input, elem);
		return;
	}

	//------------------------------------------------------------------------------
	// non-flat. Read header.
	CborToken token = decodeCborToken(input);
	bool definite = !(token.type & 0b001);

	if (definite) {
		static if (dynamicArray)
		{
			ulong lengthToRead = token.uinteger;
			checkArraySize(lengthToRead);
			if (outValue.length != cast(size_t)lengthToRead)
				outValue.length = cast(size_t)token.uinteger;
			foreach (ref elem; outValue)
				decodeCbor!(duplicate, flatten)(input, elem);
			return;
		}
		else static if (staticArray)
		{
			foreach (ref elem; outValue)
				decodeCbor!(duplicate, flatten)(input, elem);
			return;
		}
	}
	else // indefinite
	{
		static if (dynamicArray)
		{
			alias Unqualified = Unqual!(ElementEncodingType!T)[];
			Unqualified output;
			output.length = 1;
			size_t i;

			while (true) {
				token = decodeCborToken(input.save);
				if (token.type == CborTokenType.breakCode) {
					import std.range : dropExactly;
					dropExactly(input, 1);
					break;
				} else {
					if (output.length == i)
						output.length = output.length * 2; // TODO allocation
					decodeCbor!(No.Duplicate, flatten)(input, output[i]);
				}
				++i;
			}
			output.length = i;
			outValue = cast(T)output;
			return;
		}
		else static if (staticArray)
			assert(false, "TODO");
	}
	assert(false, "TODO");
}

private void decodeCborExactByteArray(
	Flag!"Duplicate" duplicate = Yes.Duplicate,
	Flag!"Flatten" flatten = No.Flatten,
	R,
	T)
	(auto ref R input, ref T outValue)
		if(isInputRange!R && is(ElementType!R == ubyte))
{
	static if (needsFlattening!(T, flatten))
	{
		outValue[] = cast(ElementEncodingType!T[])(readBytes(input, T.length));
		return;
	}
	else
	{
		CborToken token = decodeCborToken(input);
		if (token.type == CborTokenType.bytesHeader)
		{
			ubyte[] data = readBytes(input, token.uinteger);
			static if (isDynamicArray!T) {
				outValue = cast(T)data;
				static if (duplicate)
					outValue = outValue.dup;
			} else static if (isStaticArray!T) {
				if (data.length != T.length)
					onCastErrorToFrom!T(token.type);
				outValue[] = data;
			} else {
				static assert(false);
			}
			return;
		}
		else if (token.type == CborTokenType.bytesIndefiniteHeader)
		{
			import std.algorithm : copy;
			import std.array : Appender, appender;

			static if (isArray!T) {
				alias unqualified = Unqual!(ElementEncodingType!T)[];
				Appender!(unqualified) sink = appender(cast(unqualified)outValue);
				sink.clear();
			} else
				alias sink = outValue;

			while (true) {
				token = decodeCborToken(input);
				if (token.type == CborTokenType.breakCode) {
					break;
				} else if (token.type == CborTokenType.bytesHeader) {
					copy(readBytes(input, token.uinteger), sink);
				} else {
					throw new CborException(
						format("Unexpected token inside indefinite-length byte array: %s; "~
							"Expected definite-length byteString", token));
				}
			}

			static if (isDynamicArray!T) {
				outValue = cast(T)sink.data;
			} else static if (isStaticArray!T) {
				outValue[] = sink.data;
			} else
				static assert(false);

			return;
		}
	}
}

private void decodeAggregate(
	Flag!"Duplicate" duplicate = Yes.Duplicate,
	Flag!"Flatten" flatten = No.Flatten, R, T)
	(auto ref R input, ref T outValue)
		if(isInputRange!R && is(ElementType!R == ubyte))
{
	CborToken token = decodeCborToken(input);

	static if (is(T == class))
	if (token.type == CborTokenType.nil) {
		outValue = null;
		return;
	}

	static if (!needsFlattening!(T, flatten))
	{
		if (token.type != CborTokenType.arrayHeader)
			throw new CborException(format("Can not decode %s from %s", T.stringof, token.type));

		size_t numMembers;

		static if (isTuple!T)
			numMembers = T.Types.length;
		else
			numMembers = numEncodableMembers!T;

		if (token.uinteger != numMembers)
		{
			throw new CborException(
				format("The number of deserialized members of %s is mismatched."~
					" Got %s, while expected %s members",
						T.stringof, token.uinteger, numMembers));
		}
	}

	static if (isTuple!T)
	{
		foreach (i, Type; T.Types)
			decodeCbor!(duplicate, flatten)(input, outValue.field[i]);
	}
	else 
	{
		static if (is(T == class))
		if (outValue is null)
			outValue = newClassInstance!T();

		foreach(i, ref member; outValue.tupleof)
		{
			static if (isEncodedField!(typeof(member)))
				decodeCbor!(duplicate, flatten)(input, outValue.tupleof[i]);
		}
	}
}

/// Decodes single cbor value and tries to convert it to requested type.
/// If types don't match CborException is thrown.
/// Note, that ubyte[] and string types are slices of input range if ubyte[] was provided.
/// Will consume input range, decoding all the elements of T.
T decodeCborSingle(T, Flag!"Flatten" flatten = No.Flatten, R)(auto ref R input)
	if(isInputRange!R && is(ElementType!R == ubyte))
{
	Unqual!T value;
	decodeCbor!(No.Duplicate, flatten)(input, value);
	return cast(T)value;
}

/// Decodes single cbor value and tries to convert it to requested type.
/// If types don't match CborException is thrown.
/// Note, that this version will dup all array slices for you.
/// Will consume input range, decoding all the elements of T.
T decodeCborSingleDup(T, Flag!"Flatten" flatten = No.Flatten, R)(auto ref R input)
	if(isInputRange!R && is(ElementType!R == ubyte))
{
	Unqual!T value;
	decodeCbor!(Yes.Duplicate, flatten)(input, value);
	return cast(T)value;
}

//------------------------------------------------------------------------------
//		HH   HH EEEEEEE LL      PPPPPP  EEEEEEE RRRRRR   SSSSS  
//		HH   HH EE      LL      PP   PP EE      RR   RR SS      
//		HHHHHHH EEEEE   LL      PPPPPP  EEEEE   RRRRRR   SSSSS  
//		HH   HH EE      LL      PP      EE      RR  RR       SS 
//		HH   HH EEEEEEE LLLLLLL PP      EEEEEEE RR   RR  SSSSS  
//------------------------------------------------------------------------------

private void putChecked(R, E)(ref R sink, const auto ref E e)
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
	import std.algorithm : copy;
	import std.bitmanip : bigEndianToNative;
	import std.range : dropExactly, take;
	
	static assert(T.sizeof == size);
	static assert(size > 0);
	if (input.length < size) onInsufficientInput();

	ubyte[size] data;

	copy(take(input, size), data[]);
	input = input.dropExactly(size);
	T result = bigEndianToNative!(T, size)(data);

	return result;
}

// Reads byte array from input range. On 32-bit can read up to uint.max bytes.
// If ubyte[] is passed as input, a slice will be returned.
// Make sure to dup array when input buffer is reused.
ubyte[] readBytes(R)(auto ref R input, ulong length)
	if(isInputRange!R && is(ElementType!R == ubyte))
{
	import std.array;
	import std.range : take;
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
private T[] readTypedArray(T, R)(auto ref R input, ulong length)
	if(isInputRange!R && is(ElementType!R == ubyte))
{
	import std.array : uninitializedArray;
	size_t arrayLength = cast(size_t)length;
	T[] result = uninitializedArray!(T[])(arrayLength);
	foreach(ref elem; result)
	{
		decodeCbor(input, elem);
	}

	return result;
}

private void checkArraySize(T)(T length)
{
	static if (size_t.sizeof < ulong.sizeof)
		if (length > size_t.max)
			throw new CborException(format("Array size is too big: %s > size_t.max", length));
}

private import std.numeric : CustomFloat;
private union __HalfRep { CustomFloat!16 h; ushort u; string toString(){return format("__HalfRep(%s, %x)", h, u);};}
private union __FloatRep { float f; uint u; string toString(){return format("__FloatRep(%s, %x)", f, u);};}
private union __DoubleRep { double d; ulong u; string toString(){return format("__DoubleRep(%s, %x)", d, u);};}

/// Outputs textual representation of cbor stream into sink or stdout if not provided.
void printCborStream(string singleIndent="  ", R)(auto ref R input)
{
	import std.stdio : stdout;
	auto writer = stdout.lockingTextWriter;
	printCborStream!singleIndent(input, writer);
}

/// ditto
void printCborStream(string singleIndent="  ", Sink, R)(
		auto ref R input,
		auto ref Sink sink,
		ulong numItems = ulong.max,
		string indent = null
	)
	if(isInputRange!R && is(ElementType!R == ubyte) && isOutputRange!(Sink, char))
{
	import std.format : formattedWrite;

	while(input.length > 0 && numItems > 0)
	{
		CborToken token = decodeCborToken(input);
		auto type = token.type;
		final switch(type) with(CborTokenType)
		{
			case boolean: formattedWrite(sink, "%s(%s)\n", indent, token.boolean); break;
			case nil: formattedWrite(sink, "%s(null)\n", indent); break;
			case undefined: formattedWrite(sink, "%s(undefined)\n", indent); break;
			case tag:
				formattedWrite(sink, "%s(tag, %s)\n", indent, token.uinteger);
				printCborStream!singleIndent(input, sink, 1, indent~singleIndent);
				break;
			case simple: formattedWrite(sink, "%s(simple, %s)\n", indent, token.uinteger); break;
			case breakCode:
				formattedWrite(sink, "%s(break)\n", indent, token.uinteger);
				return;
			case posinteger: formattedWrite(sink, "%s(posinteger, %s)\n", indent, token.uinteger); break;
			case neginteger: formattedWrite(sink, "%s(neginteger, %s)\n", indent, token.integer); break;
			case floating: formattedWrite(sink, "%s(floating, %s)\n", indent, token.floating); break;
			case arrayHeader:
				formattedWrite(sink, "%s(array, %s)\n", indent, token.uinteger);
				printCborStream!singleIndent(input, sink, token.uinteger, indent~singleIndent);
				break;
			case arrayIndefiniteHeader:
				formattedWrite(sink, "%s(array, _)\n", indent);
				printCborStream!singleIndent(input, sink, ulong.max, indent~singleIndent);
				break;
			case mapHeader:
				formattedWrite(sink, "%s(map, %s)\n", indent, token.uinteger);
				printCborStream!singleIndent(input, sink, token.uinteger, indent~singleIndent);
				break;
			case mapIndefiniteHeader:
				formattedWrite(sink, "%s(map, _)\n", indent);
				printCborStream!singleIndent(input, sink, ulong.max, indent~singleIndent);
				break;
			case bytesHeader:
				formattedWrite(sink, "%s(bytes, %s)\n%s%s(%(%02x%))\n",
					indent, token.uinteger, indent, singleIndent, readBytes(input, token.uinteger));
				break;
			case bytesIndefiniteHeader:
				formattedWrite(sink, "%s(bytes, _)\n", indent);
				printCborStream!singleIndent(input, sink, ulong.max, indent~singleIndent);
				break;
			case textHeader:
				formattedWrite(sink, "%s(text, %s)\n%s%s\"%s\"\n",
					indent, token.uinteger, indent, singleIndent, cast(string)readBytes(input, token.uinteger));
				break;
			case textIndefiniteHeader:
				formattedWrite(sink, "%s(text, _)\n", indent);
				printCborStream!singleIndent(input, sink, ulong.max, indent~singleIndent);
				break;
		}
		--numItems;
	}
}

private template isEncodedField(T)
{
	enum isEncodedField = isIntegral!T || isFloatingPoint!T || isBoolean!T ||
		is(Unqual!T == typeof(null)) || isArray!T || isInputRange!T ||
		isTuple!T || is(T == string) || is(T == class) || is(T == struct) ||
		isAssociativeArray!T;
}

/// Tests if type can be encoded in flat mode, i.e. without header
private template canBeFlattened(T)
{
	enum bool canBeFlattened =
		isStaticArray!T ||
		(isTuple!T && isExpressionTuple!T) ||
		is(T == struct);
}

private enum bool needsFlattening(T, Flag!"Flatten" flatten) = canBeFlattened!T && flatten;

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

private C newClassInstance(C)() if (is(C == class))
{
	import core.memory : GC;
	void* memory = GC.malloc(typeid(C).init.length);
	memory[0 .. typeid(C).init.length] = typeid(C).init[];
	return cast(C) memory;
}

//------------------------------------------------------------------------------
//		TTTTTTT EEEEEEE  SSSSS  TTTTTTT  SSSSS  
//		  TTT   EE      SS        TTT   SS      
//		  TTT   EEEEE    SSSSS    TTT    SSSSS  
//		  TTT   EE           SS   TTT        SS 
//		  TTT   EEEEEEE  SSSSS    TTT    SSSSS  
//------------------------------------------------------------------------------

// Testing helpers
version(unittest)
{
	import std.array : Appender;
	import std.stdio;
	Appender!(ubyte[]) testBuf;

	CborToken getTokenAndReset()
	{
		CborToken res = decodeCborToken(testBuf.data);
		testBuf.clear();
		return res;
	}

	void assertToken(CborToken expected)
	{
		CborToken decoded = getTokenAndReset();
		ubyte[] decodedRep = (cast(ubyte*)(&decoded))[0..decoded.sizeof];
		ubyte[] expectedRep = (cast(ubyte*)(&expected))[0..expected.sizeof];
		//writefln("%s %s", decodedRep, expectedRep);
		assert(decoded == expected, format("decoded %s, %s, expected %s, %s",
				decoded, decodedRep, expected, expectedRep));
	}

	void assertHexAndClear(string file = __MODULE__, size_t line = __LINE__)(string hex)
	{
		assertEqual!(file, line)(toHexString(testBuf.data), hex);
		testBuf.clear();
	}

	void printBufferAndReset()
	{
		import std.stdio : stdout;
		printCborStream(testBuf.data);
		testBuf.clear();
	}

	void assertEqual(string file = __MODULE__, size_t line = __LINE__, A, B)(A a, B b)
	{
		assert(a == b, format("%s(%s) != %s(%s) at %s:%s", typeid(a), a, typeid(b), b, file, line));
	}

	void assertf(Args...)(bool condition, string formatStr, Args args)
	{
		assert(condition, format(formatStr, args));
	}

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
// Basic encoding/decoding
unittest
{
	// positive integer
	encodeCborInt(testBuf, 1);
	assertToken(CborToken(CborTokenType.posinteger, 1));
	// negative integer
	encodeCborInt(testBuf, -10);
	assertToken(CborToken(CborTokenType.neginteger, -10));
	// bool true
	encodeCborBool(testBuf, true);
	assertToken(CborToken(CborTokenType.boolean, true));
	// bool false
	encodeCborBool(testBuf, false);
	assertToken(CborToken(CborTokenType.boolean, false));
	// null
	encodeCborNull(testBuf);
	assertToken(CborToken(CborTokenType.nil));
	// undefined
	encodeCborUndefined(testBuf);
	assertToken(CborToken(CborTokenType.undefined));
	// break
	encodeCborBreak(testBuf);
	assertToken(CborToken(CborTokenType.breakCode));
	// simple
	encodeCborSimple(testBuf, 10);
	assertToken(CborToken(CborTokenType.simple, 10));
	// bytes
	encodeCborBytesHeader(testBuf, 10);
	assertToken(CborToken(CborTokenType.bytesHeader, 10));
	// bytes
	encodeCborBytesHeader(testBuf);
	assertToken(CborToken(CborTokenType.bytesIndefiniteHeader));
	// text
	encodeCborStringHeader(testBuf, 10);
	assertToken(CborToken(CborTokenType.textHeader, 10));
	// text
	encodeCborStringHeader(testBuf);
	assertToken(CborToken(CborTokenType.textIndefiniteHeader));
	// array
	encodeCborArrayHeader(testBuf, 10);
	assertToken(CborToken(CborTokenType.arrayHeader, 10));
	// array
	encodeCborArrayHeader(testBuf);
	assertToken(CborToken(CborTokenType.arrayIndefiniteHeader));
	// map
	encodeCborMapHeader(testBuf, 10);
	assertToken(CborToken(CborTokenType.mapHeader, 10));
	// map
	encodeCborMapHeader(testBuf);
	assertToken(CborToken(CborTokenType.mapIndefiniteHeader));
	// tag
	encodeCborTag(testBuf, 1);
	assertToken(CborToken(CborTokenType.tag, 1));
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
	cmpEncoded([1:2], "0xa10102");

	cmpEncoded(cast(ubyte)42, "0x182a");
	cmpEncoded(cast(ushort)42, "0x182a");
	cmpEncoded(cast(uint)42, "0x182a");
	cmpEncoded(cast(ulong)42, "0x182a");
	cmpEncoded(cast(byte)42, "0x182a");
	cmpEncoded(cast(short)42, "0x182a");
	cmpEncoded(cast(int)42, "0x182a");
	cmpEncoded(cast(long)42, "0x182a");
}

// indefinite-length encoding
unittest
{
	// {"a": 1, "b": [2, 3]}
	encodeCborMapHeader(testBuf, 2);
		encodeCborString(testBuf, "a");
		encodeCborInt(testBuf, 1);
		encodeCborString(testBuf, "b");
		encodeCborArrayHeader(testBuf, 2);
			encodeCborInt(testBuf, 2);
			encodeCborInt(testBuf, 3);
	assertHexAndClear("0xa26161016162820203");

	// ["a", {"b": "c"}]
	encodeCborArrayHeader(testBuf, 2);
		encodeCborString(testBuf, "a");
		encodeCborMapHeader(testBuf, 1);
			encodeCborString(testBuf, "b");
			encodeCborString(testBuf, "c");
	assertHexAndClear("0x826161a161626163");

	// ["a": "A", "b": "B", "c":"C", "d": "D", "e": "E"]
	encodeCborMapHeader(testBuf, 5);
		foreach(chr; "aAbBcCdDeE")
			encodeCborString(testBuf, (&chr)[0..1]);
	assertHexAndClear("0xa56161614161626142616361436164614461656145");

	// (_ h'0102', h'030405')
	encodeCborBytesHeader(testBuf);
		encodeCborBytes(testBuf, cast(ubyte[])[1, 2]);
		encodeCborBytes(testBuf, cast(ubyte[])[3, 4, 5]);
		encodeCborBreak(testBuf);
	assertHexAndClear("0x5f42010243030405ff");

	// (_ "strea", "ming")
	encodeCborStringHeader(testBuf);
		encodeCborString(testBuf, "strea");
		encodeCborString(testBuf, "ming");
		encodeCborBreak(testBuf);
	assertHexAndClear("0x7f657374726561646d696e67ff");

	// [_ ]
	encodeCborArrayHeader(testBuf);
		encodeCborBreak(testBuf);
	assertHexAndClear("0x9fff");

	// [_ 1, [2, 3], [_ 4, 5]]
	encodeCborArrayHeader(testBuf);
		encodeCborInt(testBuf, 1);
		encodeCborArrayHeader(testBuf, 2);
			encodeCborInt(testBuf, 2);
			encodeCborInt(testBuf, 3);
		encodeCborArrayHeader(testBuf);
			encodeCborInt(testBuf, 4);
			encodeCborInt(testBuf, 5);
			encodeCborBreak(testBuf);
		encodeCborBreak(testBuf);
	assertHexAndClear("0x9f018202039f0405ffff");

	// [_ 1, [2, 3], [4, 5]]
	encodeCborArrayHeader(testBuf);
		encodeCborInt(testBuf, 1);
		encodeCborArrayHeader(testBuf, 2);
			encodeCborInt(testBuf, 2);
			encodeCborInt(testBuf, 3);
		encodeCborArrayHeader(testBuf, 2);
			encodeCborInt(testBuf, 4);
			encodeCborInt(testBuf, 5);
		encodeCborBreak(testBuf);
	assertHexAndClear("0x9f01820203820405ff");

	// [1, [2, 3], [_ 4, 5]]
	encodeCborArrayHeader(testBuf, 3);
		encodeCborInt(testBuf, 1);
		encodeCborArrayHeader(testBuf, 2);
			encodeCborInt(testBuf, 2);
			encodeCborInt(testBuf, 3);
		encodeCborArrayHeader(testBuf);
			encodeCborInt(testBuf, 4);
			encodeCborInt(testBuf, 5);
			encodeCborBreak(testBuf);
	assertHexAndClear("0x83018202039f0405ff");

	// [1, [_ 2, 3], [4, 5]]
	encodeCborArrayHeader(testBuf, 3);
		encodeCborInt(testBuf, 1);
		encodeCborArrayHeader(testBuf);
			encodeCborInt(testBuf, 2);
			encodeCborInt(testBuf, 3);
			encodeCborBreak(testBuf);
		encodeCborArrayHeader(testBuf, 2);
			encodeCborInt(testBuf, 4);
			encodeCborInt(testBuf, 5);
	assertHexAndClear("0x83019f0203ff820405");

	import std.range : iota;
	import std.algorithm : each;
	// [_ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25],
	encodeCborArrayHeader(testBuf);
		iota(1, 26).each!(a => encodeCborInt(testBuf, a));
		encodeCborBreak(testBuf);
	assertHexAndClear("0x9f0102030405060708090a0b0c0d0e0f101112131415161718181819ff");

	// {_ "a": 1, "b": [_ 2, 3]}
	encodeCborMapHeader(testBuf);
		encodeCborString(testBuf, "a");
		encodeCborInt(testBuf, 1);
		encodeCborString(testBuf, "b");
		encodeCborArrayHeader(testBuf);
			encodeCborInt(testBuf, 2);
			encodeCborInt(testBuf, 3);
			encodeCborBreak(testBuf);
		encodeCborBreak(testBuf);
	assertHexAndClear("0xbf61610161629f0203ffff");

	// ["a", {_ "b": "c"}]
	encodeCborArrayHeader(testBuf, 2);
		encodeCborString(testBuf, "a");
		encodeCborMapHeader(testBuf);
			encodeCborString(testBuf, "b");
			encodeCborString(testBuf, "c");
			encodeCborBreak(testBuf);
	assertHexAndClear("0x826161bf61626163ff");

	// {_ "Fun": true, "Amt": -2}
	encodeCborMapHeader(testBuf);
		encodeCborString(testBuf, "Fun");
		encodeCborBool(testBuf, true);
		encodeCborString(testBuf, "Amt");
		encodeCborInt(testBuf, -2);
		encodeCborBreak(testBuf);
	assertHexAndClear("0xbf6346756ef563416d7421ff");
}

// indefinite-length decoding
unittest
{
	encodeCborBytesHeader(testBuf);
		encodeCborBytes(testBuf, cast(ubyte[])[1, 2]);
		encodeCborBytes(testBuf, cast(ubyte[])[1, 2]);
		encodeCborBreak(testBuf);
	ubyte[] output1;
	decodeCbor(testBuf.data, output1);
	assertEqual(output1, [1,2,1,2]);
	testBuf.clear();

	encodeCborStringHeader(testBuf);
		encodeCborString(testBuf, "zzz");
		encodeCborString(testBuf, "zzz");
		encodeCborString(testBuf, "zzz");
		encodeCborBreak(testBuf);
	string output2;
	decodeCbor(testBuf.data, output2);
	assertEqual(output2, "zzzzzzzzz");
	testBuf.clear();

	encodeCborArrayHeader(testBuf);
		encodeCborInt(testBuf, 1);
		encodeCborInt(testBuf, 2);
		encodeCborInt(testBuf, 3);
		encodeCborInt(testBuf, 4);
		encodeCborInt(testBuf, 5);
		encodeCborBreak(testBuf);
	int[] output3;
	decodeCbor(testBuf.data, output3);
	assertEqual(output3, [1,2,3,4,5]);
	testBuf.clear();

	encodeCborMapHeader(testBuf);
		encodeCborInt(testBuf, 1);
		encodeCborString(testBuf, "abc");
		encodeCborInt(testBuf, 2);
		encodeCborString(testBuf, "def");
		encodeCborBreak(testBuf);
	string[int] output4;
	decodeCbor(testBuf.data, output4);
	assertEqual(output4, [1:"abc", 2:"def"]);
	testBuf.clear();
}

// Printing
unittest
{
	Appender!(char[]) strBuf; strBuf.reserve(1024);
	void cmpStreamStringAndReset(string file = __MODULE__, size_t line = __LINE__)(string expected){
		assertEqual!(file, line)(strBuf.data, expected);
		testBuf.clear();
		strBuf.clear();
	}
	import std.range : cycle, take, takeExactly, repeat, Repeat, iota;

	encodeCborInt(testBuf, 1);
	printCborStream(testBuf.data, strBuf);
	cmpStreamStringAndReset("(posinteger, 1)\n");
	
	encodeCborArrayHeader(testBuf);
		encodeCborInt(testBuf, 1);
		encodeCborArrayHeader(testBuf, 2);
			encodeCborInt(testBuf, 2);
			encodeCborInt(testBuf, 3);
		encodeCborArrayHeader(testBuf);
			encodeCborInt(testBuf, 4);
			encodeCborInt(testBuf, 5);
			encodeCborBreak(testBuf);
		encodeCborBreak(testBuf);

	printCborStream(testBuf.data, strBuf);
	cmpStreamStringAndReset(
	"(array, _)\n"
	"  (posinteger, 1)\n"
	"  (array, 2)\n"
	"    (posinteger, 2)\n"
	"    (posinteger, 3)\n"
	"  (array, _)\n"
	"    (posinteger, 4)\n"
	"    (posinteger, 5)\n"
	"    (break)\n"
	"  (break)\n");

	encodeCborTag(testBuf, 100);
		encodeCborBytesHeader(testBuf, 16);
			auto bytes = iota!ubyte(10).cycle.take(16);
			encodeCborBytesItems(testBuf, bytes);

	printCborStream(testBuf.data, strBuf);
	cmpStreamStringAndReset(
	"(tag, 100)\n"~
	"  (bytes, 16)\n"~
	"    (00010203040506070809000102030405)\n");
}

/// structs, classes, tuples
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
		int[] intarr;
		string str;
		Inner inner;

		void fun(){} // not encoded
		void* pointer; // not encoded
	}

	ubyte[1024] buf1;
	size_t size;

	Test1 test = Test1(42, -120, 111111, -123456789, 0.1234, -0.987654,
		cast(ubyte[])[1,2,3,4,5,6,7,8], [42, 41], "It is a test string",
		Inner([1,2,3,4,5], "Test of inner struct"));

	size = encodeCborArray(buf1[], test);
	Test1 result = decodeCborSingle!Test1(buf1[0..size]);
	assertEqual(test, result);

	import std.typecons : Tuple;

	alias TupleVal = Tuple!(int, string, byte, string);
	auto testTuple = TupleVal(1, "hello", 56, "there");

	size = encodeCborArray(buf1[], testTuple);
	TupleVal resultTuple = decodeCborSingle!TupleVal(buf1[0..size]);
	assertEqual(testTuple, resultTuple);

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
	assertEqual(encodedBuf.length, 0);

	foreach(i, m; resultClass.tupleof)
		assertEqual(testClass.tupleof[i], m);

	testClass.inner = new Inner2;
	testClass.inner.val = -555;
	testClass.inner.u = 123456789;

	size = encodeCborArray(buf1[], testClass);
	resultClass = decodeCborSingle!Test2(buf1[0..size]);

	foreach(i, m; resultClass.inner.tupleof)
		assertEqual(testClass.inner.tupleof[i], m);
}

unittest // decoding with dup
{
	ubyte[128] buf1;
	size_t size;

	// with dup
	size = encodeCbor(buf1[], cast(ubyte[])[0, 1, 2, 3, 4, 5]);
	ubyte[] data = decodeCborSingleDup!(ubyte[])(buf1[0..size]);
	buf1[] = 0; // zero-out initial data, to check that result was duped.

	assertEqual(data, [0, 1, 2, 3, 4, 5]);

	// without dup
	size = encodeCbor(buf1[], cast(ubyte[])[0, 1, 2, 3, 4, 5]);
	data = decodeCborSingle!(ubyte[])(buf1[0..size]);
	buf1[] = 0;

	assertEqual(data, [0, 0, 0, 0, 0, 0]);

	// dup is only needed for ubyte[] and string types,
	// because they can be sliced from ubyte[] input range
	size = encodeCbor(buf1[], [0, 1, 2, 3, 4, 5]); // int array
	int[] intData = decodeCborSingle!(int[])(buf1[0..size]); // no dup
	buf1[] = 0;

	assertEqual(intData, [0, 1, 2, 3, 4, 5]);

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
	assertEqual(data1, [0, 1, 2, 3, 4, 5]);

	// regular static array
	size = encodeCbor(buf[], cast(int[6])[0, 1, 2, 3, 4, 5]);
	int[6] data2 = decodeCborSingle!(int[6])(buf[0..size]);
	assertEqual(data2, [0, 1, 2, 3, 4, 5]);
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

	assertEqual(testData, decodeCborSingle!(ubyte[])(buffer.data));
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
	assertEqual(str1, "abc");

	size = encodeCbor(buf[], cast(const char[])"abc");
	const char[] str2 = decodeCborSingle!(const char[])(buf[0..size]);
	assertEqual(str2, "abc");

	size = encodeCbor(buf[], cast(immutable char[])"abc");
	immutable char[] str3 = decodeCborSingle!(immutable char[])(buf[0..size]);
	assertEqual(str3, "abc");
}

unittest // char wchar dchar
{
	ubyte[1024] buf;
	char testChar = 'c';

	size_t size = encodeCbor(buf[], cast(char)testChar);
	char chr = decodeCborSingle!(char)(buf[0..size]);
	assertEqual(chr, testChar);

	size = encodeCbor(buf[], cast(wchar)testChar);
	wchar wchr = decodeCborSingle!(wchar)(buf[0..size]);
	assertEqual(wchr, testChar);

	size = encodeCbor(buf[], cast(dchar)testChar);
	dchar dchr = decodeCborSingle!(dchar)(buf[0..size]);
	assertEqual(dchr, testChar);

	size = encodeCbor(buf[], cast(const char)testChar);
	const char constchr = decodeCborSingle!(const char)(buf[0..size]);
	assertEqual(constchr, testChar);

	size = encodeCbor(buf[], cast(const wchar)testChar);
	const wchar constwchr = decodeCborSingle!(const wchar)(buf[0..size]);
	assertEqual(constwchr, testChar);

	size = encodeCbor(buf[], cast(immutable dchar)testChar);
	immutable dchar immdchr = decodeCborSingle!(immutable dchar)(buf[0..size]);
	assertEqual(immdchr, testChar);
}

unittest // wstring dstring; static char wchar dchar arrays
{
	ubyte[1024] buf;

	size_t size = encodeCbor(buf[], "hello w"w);
	wstring wstr = decodeCborSingle!(wstring)(buf[0..size]);
	assertEqual(wstr, "hello w"w);

	size = encodeCbor(buf[], "hello d"d);
	dstring dstr = decodeCborSingle!(dstring)(buf[0..size]);
	assertEqual(dstr, "hello d"d);

	size = encodeCbor(buf[], cast(char[7])"hello c");
	char[7] str1 = decodeCborSingle!(char[7])(buf[0..size]);
	assertEqual(str1, "hello c");

	size = encodeCbor(buf[], cast(wchar[7])"hello w");
	wchar[7] wstr1 = decodeCborSingle!(wchar[7])(buf[0..size]);
	assertEqual(wstr1, "hello w"w);

	size = encodeCbor(buf[], cast(dchar[7])"hello d");
	dchar[7] dstr1 = decodeCborSingle!(dchar[7])(buf[0..size]);
	assertEqual(dstr1, "hello d"d);
}

unittest // char[] wchar[] dchar[]
{
	ubyte[1024] buf;

	size_t size = encodeCbor(buf[], cast(char[])"hello");
	char[] str1 = decodeCborSingle!(char[])(buf[0..size]);
	assertEqual(str1, "hello");

	size = encodeCbor(buf[], cast(wchar[])"hello");
	wchar[] wstr1 = decodeCborSingle!(wchar[])(buf[0..size]);
	assertEqual(wstr1, "hello"w);

	size = encodeCbor(buf[], cast(dchar[])"hello");
	dchar[] dstr1 = decodeCborSingle!(dchar[])(buf[0..size]);
	assertEqual(dstr1, "hello"d);
}

unittest // flatten mode
{
	ubyte[1024] buf;
	size_t size;

	size = encodeCbor(buf[], cast(ubyte[2])[1, 2]);
	assertEqual(toHexString(buf[0..size]), "0x420102");

	// Raw
	size = encodeCbor!(Yes.Flatten)(buf[], cast(ubyte[2])[1, 2]);
	assertEqual(toHexString(buf[0..size]), "0x0102");

	// Array
	size = encodeCbor!(Yes.Flatten)(buf[], cast(int[2])[1, 2]);
	assertEqual(toHexString(buf[0..size]), "0x0102");

	// String
	size = encodeCbor!(Yes.Flatten)(buf[], cast(immutable(char)[1])"a");
	assertEqual(toHexString(buf[0..size]), "0x61");

	// Tuple
	import std.typecons : tuple;
	size = encodeCbor!(Yes.Flatten)(buf[], tuple(1, 2));
	assertEqual(toHexString(buf[0..size]), "0x0102");

	// Struct
	static struct A {
		int a, b;
	}
	static struct B {
		A a;
	}
	static struct C {
		B b;
	}
	static struct D {
		C c;
	}
	size = encodeCbor!(Yes.Flatten)(buf[], D(C(B(A(1, 2)))));
	assertEqual(toHexString(buf[0..size]), "0x0102");

	size = encodeCbor!(No.Flatten)(buf[], D(C(B(A(1, 2)))));
	assertEqual(toHexString(buf[0..size]), "0x818181820102");

	size = encodeCbor!(Yes.Flatten)(buf[], cast(int)1);
	assertEqual(decodeCborSingle!(int, Yes.Flatten)(buf[0..size]), 1);

	size = encodeCbor!(Yes.Flatten)(buf[], cast(float)1);
	assertEqual(decodeCborSingle!(float, Yes.Flatten)(buf[0..size]), 1);

	size = encodeCbor!(Yes.Flatten)(buf[], true);
	assertEqual(decodeCborSingle!(bool, Yes.Flatten)(buf[0..size]), true);

	size = encodeCbor!(Yes.Flatten)(buf[], 'a');
	assertEqual(decodeCborSingle!(char, Yes.Flatten)(buf[0..size]), 'a');

	size = encodeCbor!(Yes.Flatten)(buf[], cast(ubyte[])[1,2]);
	assertEqual(decodeCborSingle!(ubyte[], Yes.Flatten)(buf[0..size]), cast(ubyte[])[1,2]);

	size = encodeCbor!(Yes.Flatten)(buf[], cast(ubyte[2])[1,2]);
	assertEqual(decodeCborSingle!(ubyte[2], Yes.Flatten)(buf[0..size]), cast(ubyte[2])[1,2]);

	// TODO
}

//------------------------------------------------------------------------------
//	EEEEEEE XX    XX  CCCCC  EEEEEEE PPPPPP  TTTTTTT IIIII  OOOOO  NN   NN 
//	EE       XX  XX  CC    C EE      PP   PP   TTT    III  OO   OO NNN  NN 
//	EEEEE     XXXX   CC      EEEEE   PPPPPP    TTT    III  OO   OO NN N NN 
//	EE       XX  XX  CC    C EE      PP        TTT    III  OO   OO NN  NNN 
//	EEEEEEE XX    XX  CCCCC  EEEEEEE PP        TTT   IIIII  OOOO0  NN   NN 
//	                                                                       
//------------------------------------------------------------------------------

class CborException : Exception
{
	@trusted pure this(string message, string file = __FILE__, size_t line = __LINE__)
	{
		super(message, file, line);
	}
}

private:

@safe pure
void onCastErrorToFrom(To)(CborTokenType from, string file = __FILE__, size_t line = __LINE__)
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