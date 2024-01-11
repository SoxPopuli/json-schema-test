// For more information see https://aka.ms/fsharp-console-apps
open Json.Schema
open Json.Schema.Generation

open System.Text.Json.Serialization
open System.Text.Json
open Json.Schema.Generation.Generators
open Json.Schema.Generation.Intents


type Nested = { x: int }
type Alias = string

type Record1 = {
    [<Minimum(10.0)>]
    IntProp: int
    [<Nullable(true)>]
    FloatProp: float
    StringProp: string
    OptionProp: int option
    [<UniqueItems(true)>]
    ListProp: int list
    RecordProp: Nested
    GuidProp: System.Guid
    DateTimeProp: System.DateTime
    [<JsonPropertyName("snake_case_prop")>]
    SnakeCaseProp: int
    AliasProp: Alias
}

let serializerOptions =
    let options = System.Text.Json.JsonSerializerOptions(WriteIndented = true)
    options.Converters.Add (
        JsonFSharpConverter (
            unionEncoding =
                (JsonUnionEncoding.ExternalTag
                 ||| JsonUnionEncoding.UnwrapOption
                 ||| JsonUnionEncoding.UnwrapFieldlessTags)
        )
    )
    options

let serialize<'T> (x: 'T) =
    System.Text.Json.JsonSerializer.Serialize<'T>(x, serializerOptions)

let deserialize<'T> (x: string): 'T =
    JsonSerializer.Deserialize<'T>(x, serializerOptions)

let getIntent<'a when 'a :> ISchemaKeywordIntent> (intents: ISchemaKeywordIntent seq) =
    let get (intent: ISchemaKeywordIntent) =
        match intent with
        | :? 'a as x -> Some x
        | _ -> None

    Seq.pick get intents


type OptionRefiner() =
    interface ISchemaRefiner with
        member _.Run(context: SchemaGenerationContextBase): unit = 
            //let typeIntent: TypeIntent = getIntent context.Intents
            let propertiesIntent: PropertiesIntent = getIntent context.Intents

            let value = propertiesIntent.Properties["Value"]

            context.Intents.Clear()
            for intent in value.Intents do
                if intent :? TypeIntent then
                    let oldType = (intent :?> TypeIntent).Type
                    context.Intents.Add(TypeIntent(SchemaValueType.Null ||| oldType))
                else
                    context.Intents.Add(intent)

        member _.ShouldRun(context: SchemaGenerationContextBase): bool = 
            context.Type.Name = "FSharpOption`1"


let schemaConfig =
    let config = SchemaGeneratorConfiguration()
    config.Refiners.Add(OptionRefiner())
    config

[<EntryPoint>]
let main _ =
    let schema = 
        JsonSchemaBuilder().FromType<Record1>(schemaConfig).Build()

    printfn "---Schema--------------------"
    let text =
        serialize schema
        
    printfn "%s" text;
    printfn "-----------------------------"

    let test = {
        Record1.IntProp = 10
        FloatProp = 1.0
        StringProp = "string"
        OptionProp = Some 2
        ListProp = [ ]
        RecordProp = { x = 3 }
        GuidProp = System.Guid.Empty
        DateTimeProp = System.DateTime(2024, 04, 02)
        SnakeCaseProp = 4
        AliasProp = "alias"
    }
    printfn "---Serialized----------------"
    let s = serialize test
    printfn "%s" s
    let nodes = System.Text.Json.Nodes.JsonNode.Parse(s)
    printfn "-----------------------------"

    printfn "---Deserialized--------------"
    let deserialized = deserialize<Record1> s
    printfn "%A" deserialized
    printfn "-----------------------------"

    printfn "---Evaluation----------------"
    let results = schema.Evaluate(nodes, EvaluationOptions(OutputFormat = OutputFormat.List))
    printfn "%s" (serialize results)
    printfn "-----------------------------"


    0
    
