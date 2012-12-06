//  Copyright 2012 Gregor Uhlenheuer
//
//  Licensed under the Apache License, Version 2.0 (the "License");
//  you may not use this file except in compliance with the License.
//  You may obtain a copy of the License at
//
//    http://www.apache.org/licenses/LICENSE-2.0
//
//  Unless required by applicable law or agreed to in writing, software
//  distributed under the License is distributed on an "AS IS" BASIS,
//  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
//  See the License for the specific language governing permissions and
//  limitations under the License.

namespace SharpXml.Common

open System

/// Attribute that allows customization of the serialization
/// and deserialization behavior of the SharpXml.XmlSerializer
[<AttributeUsage(AttributeTargets.Class ||| AttributeTargets.Property)>]
type XmlElementAttribute(name : string) =
    inherit Attribute()

    let mutable name = name
    let mutable itemName = Unchecked.defaultof<string>
    let mutable keyName = Unchecked.defaultof<string>
    let mutable valueName = Unchecked.defaultof<string>
    let mutable ns = Unchecked.defaultof<string>

    new() = XmlElementAttribute(null)

    /// Name to override the property name
    member x.Name
        with get() = name
        and set(v) = name <- v

    /// Name to override item names of collections
    member x.ItemName
        with get() = itemName
        and set(v) = itemName <- v

    /// Name to override key names of key-value collections
    member x.KeyName
        with get() = keyName
        and set(v) = keyName <- v

    /// Name to override value names of key-value collections
    member x.ValueName
        with get() = valueName
        and set(v) = valueName <- v

    /// Optional namespace to use on serialization
    member x.Namespace
        with get() = ns
        and set(v) = ns <- v
