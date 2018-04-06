#r "paket:
nuget Fake.IO.FileSystem
nuget Fake.DotNet
nuget Fake.DotNet.Cli
nuget Fake.Core.Target //"
#load "./.fake/build.fsx/intellisense.fsx"

open Fake.IO
open Fake.IO.Globbing.Operators //enables !! and globbing
open Fake.DotNet
open Fake.Core

// Properties
let configuration = DotNet.BuildConfiguration.Release
let buildDir = __SOURCE_DIRECTORY__  + 
               "/../build/".Replace('/', System.IO.Path.DirectorySeparatorChar) +
               configuration.ToString()

// Targets
Target.create "Clean" (fun _ -> 
  Trace.log " --- Cleaning stuff --- "
  Shell.CleanDir buildDir
)

Target.create "BuildApp" (fun _ ->
  Trace.log " --- Building the app --- "
  !! "src/*.sln"
  |> Seq.head
  |> DotNet.build (fun opt -> 
      { opt with 
          Configuration = configuration
          OutputPath = Some buildDir })
)

Target.create "Deploy" (fun _ ->
  Trace.log " --- Deploying app --- "
)

open Fake.Core.TargetOperators

"Clean"
  ==> "BuildApp"

// Start build
Target.runOrDefault "BuildApp"