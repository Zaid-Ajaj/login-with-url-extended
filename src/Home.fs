[<RequireQualifiedAccess>]
module App

open Elmish
open Feliz
open Feliz.Router

[<RequireQualifiedAccess>]
type Page =
  | Login of Login.State
  | Overview of Overview.State
  | Index
  | NotFound

[<RequireQualifiedAccessAttribute>]
type Url =
    | Index
    | NotFound
    | Login
    | Overview
    | Logout

let parseUrl = function
    | [  ] -> Url.Index
    | [ "login" ] -> Url.Login
    | [ "overview" ] -> Url.Overview
    | [ "logout" ] -> Url.Logout
    | _ -> Url.NotFound

type ApplicationUser =
    | Anonymous
    | LoggedIn of Api.User

type State =
  { CurrentPage : Page
    CurrentUrl  : Url
    User : ApplicationUser }

type Msg =
    | LoginMsg of Login.Msg
    | OverviewMsg of Overview.Msg
    | UrlChanged of Url
    | Logout

let init() =
    let initialUrl = parseUrl (Router.currentUrl())
    let defaultState =
        { User = Anonymous
          CurrentUrl = initialUrl
          CurrentPage = Page.Index }

    match initialUrl with
    | Url.Index ->
        defaultState, Cmd.none

    | Url.Login ->
        let loginState, loginCmd = Login.init()
        let nextPage = Page.Login loginState
        { defaultState with CurrentPage = nextPage }, Cmd.map LoginMsg loginCmd

    | Url.Overview ->
        defaultState, Router.navigate("login")

    | Url.Logout ->
        defaultState, Router.navigate("/")

    | Url.NotFound ->
        { defaultState with CurrentPage = Page.NotFound }, Cmd.none



let update (msg: Msg) (state: State) =
    match msg, state.CurrentPage with
    | LoginMsg loginMsg, Page.Login loginState ->
        match loginMsg with
        | Login.UserLoggedIn user ->
            { state with User = LoggedIn user }, Router.navigate("/")

        | loginMsg ->
            let loginState, loginCmd = Login.update loginMsg loginState
            { state with CurrentPage = Page.Login loginState }, Cmd.map LoginMsg loginCmd

    | OverviewMsg overviewMsg, Page.Overview overviewState ->
        let overviewState, overviewCmd = Overview.update overviewMsg overviewState
        { state with CurrentPage = Page.Overview overviewState }, Cmd.map OverviewMsg overviewCmd

    | UrlChanged nextUrl, _ ->
        let show page = { state with CurrentPage = page; CurrentUrl = nextUrl }

        match nextUrl with
        | Url.Index -> show Page.Index, Cmd.none
        | Url.NotFound -> show Page.NotFound, Cmd.none
        | Url.Login ->
            let login, loginCmd = Login.init()
            show (Page.Login login), Cmd.map LoginMsg loginCmd

        | Url.Overview ->
            match state.User with
            | Anonymous ->  state, Router.navigate("login")
            | LoggedIn user ->
                let overview, overviewCmd = Overview.init user
                show (Page.Overview overview), Cmd.map OverviewMsg overviewCmd

        | Url.Logout ->
            { state with User = Anonymous }, Router.navigate("/")

    | _, _ ->
        state, Cmd.none

let index (state: State) (dispatch: Msg -> unit) =
    match state.User with
    | Anonymous ->
        Html.div [
            Html.h1 "Welcome, guest"
            Html.a [
                prop.className [ "button"; "is-info" ]
                prop.style [ style.margin 5 ]
                prop.href (Router.format("login"))
                prop.text "Login"
            ]
        ]

    | LoggedIn user ->
        Html.div [
            Html.h1 (sprintf "Welcome, %s" user.Username)
            Html.a [
                prop.className [ "button"; "is-info" ]
                prop.style [ style.margin 5 ]
                prop.href (Router.format("overview"))
                prop.text "Overview"
            ]
            Html.a [
                prop.className [ "button"; "is-info" ]
                prop.style [ style.margin 5 ]
                prop.href (Router.format("logout"))
                prop.text "Logout"
            ]
        ]

let render (state: State) (dispatch: Msg -> unit) =
    let activePage =
        match state.CurrentPage with
        | Page.Login login -> Login.render login (LoginMsg >> dispatch)
        | Page.Overview overview -> Overview.render overview (OverviewMsg >> dispatch)
        | Page.Index -> index state dispatch
        | Page.NotFound -> Html.h1 "Not Found"

    Router.router [
        Router.onUrlChanged (parseUrl >> UrlChanged >> dispatch)
        Router.application [
            Html.div [
                prop.style [ style.padding 20 ]
                prop.children [ activePage ]
            ]
        ]
    ]