[<RequireQualifiedAccess>]
module Api

type AccessToken = AccessToken of string

type User =
    { Username : string
      AccessToken : AccessToken }

type LoginResult =
    | UsernameOrPasswordIncorrect
    | LoggedIn of User

let login (username: string) (password: string) =
    async {
        do! Async.Sleep 1500
        if username = "admin" && password = "admin" then
            let accessToken = System.Guid.NewGuid().ToString()
            return LoggedIn { Username = username; AccessToken = AccessToken accessToken }
        else
            return UsernameOrPasswordIncorrect
    }