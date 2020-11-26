# Design

## Overview
The idea behind `scala-jsonschema` is as simple as:

- create jsonschema model
- maintain mapping between scala types and jsonschema constructs
- provide functionality to render adt   

## Model
The key trait of the whole library is `json.Schema[T]`. 
All jsonschema constructs are mimicked as extensions to this trait.

There are:

### `string       [T]                 ( format, pattern )`  
There are many types that usually we want to map to string. 
For example: `Char`, `Date-Time` family of types, `UUID`, `URL`, `URI`, `Enums`, `IpAddress`, etc.
In JsonSchema string may be specified by using options like:

#### Format (enum)
  - `date` 
  - `time` 
  - `date-time` Date representation, as defined by RFC 3339, section 5.6.
  - `email`     Internet email address, see RFC 5322, section 3.4.1.
  - `hostname`  Internet host name, see RFC 1034, section 3.1.
  - `ipv4`      Internet host name, see RFC 1034, section 3.1.
  - `ipv6`      IPv6 address, as defined in RFC 2373, section 2.2.
  - `uri`       A universal resource identifier (URI), according to RFC3986.  

#### Pattern
   
### `number       [T]`  
- `integer`  
- `boolean`  
- `array        [T, C[_]]           ( componentSchema, unique )`  
- `dictionary   [K, V, C[_, _]]     ( valueSchema )`  
- `object       [T]                 ( fields )`  
- `enum         [T]                 ( values )`  
- `ref`  
- `oneof        [T]                 ( alternativeSchemas )`  
- `allof        [T]                 ( unitedSchemas )`  
- `not          [T]                 ( schema )`  
- `ref          [T]                 ( id, schema )`  
- `lazy-ref     [T]                 ( id )`  
- `value-class  [T]                 ( schema )`  

### Schema vs Predef
###`def` vs `ref`

## Derivation
TODO

## Integration
TODO

## Extension
TODO