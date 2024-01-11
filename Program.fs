// For more information see https://aka.ms/fsharp-console-apps
open Json.Schema
open Json.Schema.Generation

open System.Text.Json.Serialization
open System.Text.Json
open Json.Schema.Generation.Generators
open Json.Schema.Generation.Intents


type Nested = { x: int }

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
}

let serialize<'T> (x: 'T) =
    let options = System.Text.Json.JsonSerializerOptions()
    options.Converters.Add (
        JsonFSharpConverter (
            unionEncoding =
                (JsonUnionEncoding.ExternalTag
                 ||| JsonUnionEncoding.UnwrapOption
                 ||| JsonUnionEncoding.UnwrapFieldlessTags)
        )
    )

    System.Text.Json.JsonSerializer.Serialize<'T>(x, options)

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

    let text =
        serialize schema
        
    printfn "%s" text;

    0
    
