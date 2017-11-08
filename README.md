# Concise Binary Object Representation (CBOR) for D language [![Build Status](https://travis-ci.org/MrSmith33/cbor-d.svg?branch=master)](https://travis-ci.org/MrSmith33/cbor-d)

### Supported features
1. Encoding
  + integers - `ubyte ushort uint ulong byte short int long`;
  + floating numbers - `float`, `double`;
  + boolean - `bool`;
  + fixed length byte strings - `ubyte[]`;
  + fixed length UTF-8 strings - `string`;
  + fixed length arrays aka tuples. Items can be of any type;
  + fixed length maps. Pairs of key/value of any type - `K[V]`;
  + Aggregate encoding. Structs, tuples and classes - `class, Tuple, struct`. Can be encoded as both arrays and maps;
  + null values - `null`;
  + Tags;
  + Simple values

2. Decoding
  + Integers;
  + Floats (half, float, double);
  + Arrays, maps, string, byte strings;
  + Aggregates can only be decoded from arrays now;
  + null values can be decoded as classes, arrays, maps, string;
  + All unused simple values will cause CborException to be thrown;
  + Streaming support (i.e. Indefinite-Length Arrays, Maps, Byte Strings and Text Strings)
  + Tags produce corresponding token.

### Restrictions
+ While aggregates can be encoded as maps, they can not be decoded as such, only from arrays.


## API
### CBOR encoding

```D
size_t encodeCborInt (sink, value)
size_t encodeCborFloat (sink, value)
size_t encodeCborBool (sink, value)
size_t encodeCborNull (sink, value)
size_t encodeCborUndefined (sink, value)
size_t encodeCborBreak (sink, value)
size_t encodeCborSimple (sink, value)

// version without length encodes indefinite-length header.
size_t encodeCborBytesHeader(sink [, length])
size_t encodeCborStringHeader(sink [, length])
size_t encodeCborArrayHeader(sink [, length])
size_t encodeCborMapHeader(sink [, length])

size_t encodeCborBytesItems (sink, bytes)

size_t encodeCborTag (sink, value)

```

### Serialization

```D
size_t encodeCbor <flatten> (sink, value)
size_t encodeCborBytes <flatten> (sink, value)
size_t encodeCborString <flatten> (sink, value)
size_t encodeCborArray <flatten> (sink, value)
size_t encodeCborMap <flatten> (sink, value)
size_t encodeCborAggregate <withFieldName, flatten> (sink, value)

@ignore attribute
```

### CBOR Decoding

```D
enum CborTokenType {
	arrayHeader
	arrayIndefiniteHeader
	mapHeader
	mapIndefiniteHeader
	bytesHeader
	bytesIndefiniteHeader
	textHeader
	textIndefiniteHeader
	undefined
	nil
	boolean
	tag
	simple
	breakCode
	posinteger
	neginteger
	floating
}
struct CborToken {
	CborTokenType type;
	union {
		bool boolean;
		long integer;
		double floating;
		ulong uinteger;
	}
}

CborToken decodeCborToken (input)
```

### Deserialization

```D
void decodeCbor <dup, flatten> (input, ref outValue)
T decodeCborSingle <T, flatten> (input)
T decodeCborSingleDup <T, flatten> (input)

ubyte[] readBytes (input, length)
void printCborStream <indent="  "> (input, sink = stdout, numItems = ulong.max, indent = "")
```

## Usage example

```D
	import cbor;

	static struct Inner
	{
		int[] array;
		string someText;
	}

	static struct Test
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
		@ignore long ignored; // not encoded

		void fun(){} // not encoded
		void* pointer; // not encoded
		int* numPointer; // not encoded
	}

	import std.array : appender;
	auto buffer = appender!(ubyte[])();

	Test test = Test(42, -120, 111111, -123456789, 0.1234, -0.987654,
		cast(ubyte[])[1,2,3,4,5,6,7,8], "It is a test string",
		Inner([1,2,3,4,5], "Test of inner struct"), 88);

	encodeCbor(buffer, test);

	// ubyte[] and string types are slices of input ubyte[].
	Test result = decodeCborSingle!Test(buffer.data);
	// or
	// Test result;
	// decodeCbor(buffer.data, result);

	// decodeCborSingleDup can be used to auto-dup those types.

	assert(result.ignored == 0);
	result.ignored = test.ignored;
	assert(test == result);


	struct Vector4f
	{
		float x, y, z, w;
	}

	// encoded as array of 4 floats
	encodeCbor(buffer, Vector4f(1,2,3,4));
	buffer.clear();

	// Flat encoding
	// encoded as 4 floats
	encodeCbor!(Yes.Flatten)(buffer, Vector4f(1,2,3,4));

	// You need to use Yes.Flatten on both sides.
	decodeCborSingle!(Vector4f, Yes.Flatten)(buffer.data);

	// Printing cbor data (by default into stdout)
	printCborStream(buffer.data);
	// prints:
	// (floating, 1)
	// (floating, 2)
	// (floating, 3)
	// (floating, 4)

	// you can also use other sink
	auto textbuffer = appender!(char[])();
	printCborStream(buffer.data, textbuffer);
```

For more examples see unittests.