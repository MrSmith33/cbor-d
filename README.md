#Concise Binary Object Representation (CBOR) for D language

### Supported features
1. Encoding
  + integers - `ubyte ushort uint ulong byte short int long`
  + floating numbers - `float`, `double`
  + boolean - `bool`
  + fixed length byte strings - `ubyte[]`
  + fixed length UTF-8 strings - `string`;
  + fixed length arrays aka tuples. Items can be of any type;
  + fixed length maps. Pairs of key/value of any type - `K[V]`;
  + Aggregate encoding. Structs, tuples and classes - `class, Tuple, struct`. Can be encoded as both arrays and maps;
  + null values - `null`.
2. Decoding
  + Integers;
  + Floats (half, float, double);
  + Arrays, maps, string, byte strings;
  + Aggregates can only be decoded from arrays now;
  + null values can be decoded as classes, arrays, maps, string;
  + All unused simple values will cause CborException to be thrown;

### Restrictions
+ No streaming support (i.e. Indefinite-Length Arrays, Maps, Byte Strings and Text Strings)
+ Tags are ignored.
+ While aggregates can be encoded as maps, they can not be decoded as such, only from arrays.


```D
	import cbor;
	
	struct Inner
	{
		int[] array;
		string someText;
	}

	struct Test
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
		int* numPointer; // not encoded
	}

	ubyte[1024] buffer;
	size_t encodedSize;

	Test test = Test(42, -120, 111111, -123456789, 0.1234, -0.987654,
		cast(ubyte[])[1,2,3,4,5,6,7,8], "It is a test string",
		Inner([1,2,3,4,5], "Test of inner struct"));

	encodedSize = encodeCborArray(buffer[], test);

	Test result = decodeCborSingle!Test(buffer[0..encodedSize]);

	assert(test == result);
```

For more examples see unittests.