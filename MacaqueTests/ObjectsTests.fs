namespace Macaque.Tests.Objects

 open NUnit.Framework
 open FsUnit
 open Macaque
 open Macaque.Objects


  [<TestFixture>]    
  type Tests() =
    
    [<Test>]
    member t.TestStringHashKey() = 
        let hello1 = ``String``("Hello World!") :> Hashable
        let hello2 = ``String``("Hello World!") :> Hashable
        let diff1 = ``String``("Ich bin David") :> Hashable
        let diff2 = ``String``("Ich bin David") :> Hashable
        hello1.HashKey |> should equal hello2.HashKey      
        diff1.HashKey  |> should equal diff2.HashKey       
        hello1.HashKey |> should not' (equal diff1.HashKey)        

    [<Test>]
    member t.TestBooleanHashKey() =
        let true1 = Boolean(true) :> Hashable
        let true2 = Boolean(true) :> Hashable
        let false1 = Boolean(false) :> Hashable
        let false2 = Boolean(false) :> Hashable
        true1.HashKey |> should equal true2.HashKey
        false1.HashKey |> should equal false2.HashKey
        true1.HashKey |> should not' (equal false1.HashKey)

    [<Test>]
    member t.TestIntegerHashKey() =
        let one1 = Integer(1L) :> Hashable
        let one2 = Integer(1L) :> Hashable
        let two1 = Integer(2L) :> Hashable
        let two2 = Integer(2L) :> Hashable
        one1.HashKey |> should equal one2.HashKey
        two1.HashKey |> should equal two2.HashKey
        one1.HashKey |> should not' (equal two1.HashKey)

    [<Test>]
    member t.TestDifferentTypesHashKey() =
        let hello = ``String``("Hello World!") :> Hashable
        let ``true`` = Boolean(true) :> Hashable
        let one = Integer(1L) :> Hashable
        hello.HashKey |> should not' (equal ``true``.HashKey)
        hello.HashKey |> should not' (equal one.HashKey)
        ``true``.HashKey |> should not' (equal one.HashKey)
        

        
        

