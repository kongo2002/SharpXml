
# SharpXml

*SharpXml* is an independent, dependency-free and fast .NET XML serialization
library. It is written in F# and is built on .NET 4.0.

The project is inspired by the great .NET JSON serializer
[ServiceStack.Text][1].


## Lean API

The API intends to appear small and descriptive at the same time:

	// Serialization functions
	string XmlSerializer.SerializeToString<T>(T element);
	void XmlSerializer.SerializeToWriter<T>(TextWriter writer, T element);

	// Deserialization functions
	T XmlSerializer.DeserializeFromString<T>(string value);
	T XmlSerializer.DeserializeFromReader<T>(TextReader reader);
	T XmlSerializer.DeserializeFromStream<T>(Stream stream);

*T* can be any .NET POCO type. Apart from others *SharpXml* supports all basic
collection types residing in `System.Collections`, `System.Collections.Generic`
and `System.Collections.Specialized`.


### Configuration

*SharpXml* intends to work in a convention based manner meaning that there
won't be too many configuration options to change its basic (de-)serialization
behavior. A few options to modify *SharpXml's* output exist anyways:

- `XmlConfig.IncludeNullValues`: Whether to include `null` values in the
  generated/serialized output (default: `false`)

- `XmlConfig.ExcludeTypeInfo`: Whether to include additional type information
  for dynamic or anonymous types (default: `false`)

- `XmlConfig.EmitCamelCaseNames`: Whether to convert property/type names into
  camel-case output, i.e. `MyClass -> "myClass"` (default: `false`)

- `XmlConfig.WriteXmlHeader`: Whether to include a XML header sequence (`<?xml
  ... ?>`) in the serialized output (default: `false`)

- `XmlConfig.ThrowOnError`: Whether to throw an exception on deserialization
  errors or silently ignore errors (default: `false`)


### Custom serialization

Although *SharpXml* comes with built-in support of all basic .NET types there
are two ways to modify its de-/serialization behavior. You can either add
custom serialization and/or deserialization logic by registering serialization
delegates for a specified type on the static `XmlConfig` class or you modify
serialization of collections using the `XmlElementAttribute` in the
`SharpXml.Common` namespace.

Moreover the serialization and deserialization of struct types may be customized
by overriding the public `ToString()` method and/or providing a static
`ParseXml()` function.


#### Registering delegates

    /// Register a serializer delegate for the specified type
    void RegisterSerializer<T>(SerializerFunc func);

    /// Register a deserializer delegate for the specified type
    void RegisterDeserializer<T>(DeserializerFunc func);

    /// Unregister the serializer delegate for the specified type
    void UnregisterSerializer<T>();

    /// Unregister the deserializer delegate for the specified type
    void UnregisterDeserializer<T>();

    /// Clear all registered custom serializer delegates
    void ClearSerializers();

    /// Clear all registered custom deserializer delegates
    void ClearDeserializers();


#### XmlElementAttribute

The `XmlElementAttribute` in `SharpXml.Common` allows you to modify the default
serialization of .NET types using a few properties to choose from:

- `[XmlElement Name="..."]`: Override the default name of the property/class

- `[XmlElement ItemName="..."]`: Override the default name of collection's
  items (default: `"item"`)

- `[XmlElement KeyName="..."]`: Override the default name of keys in dictionary
  types (default: `"key"`)

- `[XmlElement ValueName="..."]`: Override the default name of values in
  dictionary types (default: `"value"`)


## XML format

In the following section I want to give a *short* description of the format
*SharpXml* generates and expects on deserialization.

The first thing to mention is that *public properties* are serialized and
deserialized only. Fields whether public or not are not serialized at the
moment and won't be in the future! Apart from that serialization is pretty
straight-forward and looks your XML looks like you would probably expect it too
anyway - at least from my point of view :-)


### Basic serialization

	public class MyClass
	{
		public int Foo { get; set; }
		public string Bar { get; set; }
	}

	var test = new MyClass { Foo = 144, Bar = "I like SharpXml very much" };

An instance of the class above will be serialized like the following:

	<MyClass>
		<Foo>144</Foo>
		<Bar>I like SharpXml very much</Bar>
	</MyClass>

Using `XmlConfig.EmitCamelCaseNames = true;` the generated XML output would
look like this instead:

	<myClass>
		<foo>144</foo>
		<bar>I like SharpXml very much</bar>
	</myClass>


### Collections

	public class ListClass
	{
		public int Id { get; set; }
		public List<string> Items { get; set; }
	}

	var test = new ListClass
		{
			Id = 20,
			Items = new List<string> { "one", "two" }
		};

*SharpXml* will generate the following XML:

	<ListClass>
		<Id>20</Id>
		<Items>
			<Item>one</Item>
			<Item>two</Item>
		</Items>
	</ListClass>


### Key-value collections (dictionaries)

	public class DictClass
	{
		public int Id { get; set; }
		public Dictionary<string, int> Values { get; set; }
	}

	var test = new DictClass
		{
			Id = 753,
			Values = new Dictionary<string, int>
				{
					{ "ten", 10 },
					{ "eight", 8 }
				}
		};

The serialized output by *SharpXml* looks like the following:

	<DictClass>
		<Id>753</Id>
		<Values>
			<Item>
				<Key>ten</Key>
				<Value>10</Value>
			</Item>
			<Item>
				<Key>eight</Key>
				<Value>8</Value>
			</Item>
		</Values>
	</DictClass>

**Note**: In all XML examples above indentation is added for convenience only.


### Using XmlElementAttribute

As mentioned before you can use the `XmlElementAttribute` to customize the
generated XML output which is especially useful for collection and dictionary
types.

	[XmlElement("CustomClass")]
	public class CustomDictClass
	{
		public int Id { get; set; }

		[XmlElement(ItemName="Element", KeyName="String", ValueName="Int")]
		public Dictionary<string, int> Values { get; set; }
	}

	var test = new CustomDictClass
		{
			Id = 753,
			Values = new Dictionary<string, int>
				{
					{ "ten", 10 },
					{ "eight", 8 }
				}
		};

This example shows the effect of all four available options given by the
`XmlElementAttribute`: `Name`, `ItemName`, `KeyName` and `ValueName`.

	<CustomClass>
		<Id>753</Id>
		<Values>
			<Element>
				<String>ten</String>
				<Int>10</Int>
			</Element>
			<Element>
				<String>eight</String>
				<Int>8</Int>
			</Element>
		</Values>
	</CustomClass>


### Struct types

Non-reference types like struct may provide custom implementation of the methods
`ToString()` and/or `ParseXml()` in order to customize *SharpXml's*
serialization behavior.

A typical example might look like this:

	public struct MyStruct
	{
		public int X { get; set; }
		public int Y { get; set; }

		/// <summary>
		/// Custom ToString() implementation - will be used by SharpXml
		/// </summary>
		public override string ToString()
		{
			return X + "x" + Y;
		}

		/// <summary>
		/// Custom deserialization function used by SharpXml
		/// </summary>
		public static MyStruct ParseXml(string input)
		{
			var parts = input.Split('x');

			return new MyStruct
				{
					X = int.Parse(parts[0]),
					Y = int.Parse(parts[1])
				};
		}
	}

	var test = new MyStruct { X = 200, Y = 50 };

Using the struct type described above results in the following output:

	<MyStruct>200x50</MyStruct>

Without the custom implementations the struct would be serialized like this:

	<MyStruct>
		<X>200</X>
		<Y>50</Y>
	</MyStruct>


### Custom serialization delegates

Moreover reference types can be customized by registering custom serialization
delegates to the static `XmlConfig` class using the aforementioned
`RegisterSerializer` and `RegisterDeserializer` functions.

	public class SomeClass
	{
		public double Width { get; set; }
		public double Height { get; set; }
	}

	// register custom serializer
	XmlConfig.RegisterSerializer<SomeClass>(x => return x.Width + "x" x.Height);

	// register custom deserializer
	XmlConfig.RegisterDeserializer<SomeClass>(v => {
			var parts = v.Split('x');
			return new SomeClass
				{
					Width = double.Parse(parts[0]),
					Height = double.Parse(parts[1])
				};
		});

The resulting XML will look pretty much the same as the struct example described
earlier but you can imagine the possibilities given by this approach.


## Todo

Some random things I am planning to work on in the future:

- Mono support
- Extend documentation/README
- Make `SharpXml.Common` an optional dependency
- Investigate into additional performance tweaks
- Additional unit tests


## Maintainer

*SharpXml* is written by Gregor Uhlenheuer. You can reach me at
[kongo2002@gmail.com][3]


## License

*SharpXml* is licensed under the [Apache license][2], Version 2.0

> Unless required by applicable law or agreed to in writing, software
> distributed under the License is distributed on an "AS IS" BASIS,
> WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
> See the License for the specific language governing permissions and
> limitations under the License.

[1]: http://github.com/ServiceStack/ServiceStack.Text
[2]: http://www.apache.org/licenses/LICENSE-2.0
[3]: mailto:kongo2002@gmail.com

<!-- vim: set noet ts=4 sw=4 sts=4 tw=80: -->
