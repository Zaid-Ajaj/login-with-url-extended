module Main

open Elmish
open Elmish.React

Program.mkProgram App.init App.update App.render
|> Program.withReactSynchronous "elmish-app"
|> Program.run