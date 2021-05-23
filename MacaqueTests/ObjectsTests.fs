namespace Macaque.Tests.Objects

 open NUnit.Framework
 open FsUnit
 open Macaque
 open Macaque.Objects


  [<TestFixture>]    
  type Tests() =
    
    [<Test>]
    member t.TestStringHashKey() = 
        let hello1 = ``String``("Hello World!")
        let hello2 = ``String``("Hello World!")
        let diff1 = ``String``("Ich bin David")
        let diff2 = ``String``("Ich bin David")
        hello1.HashKey |> should equal hello2.HashKey      
        diff1.HashKey  |> should equal diff2.HashKey       
        hello1.HashKey |> should not' (equal diff1.HashKey)        

    
        

        
        

