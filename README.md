Concise Binary Object Representation (CBOR) for D language


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
