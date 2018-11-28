module Page.EditUser exposing (Model, Msg(..), init, subscriptions, update, view)

import Html exposing (Html, div)
import Html.Attributes as Attributes exposing (class, classList, style, attribute)
import Html.Events as Events exposing (onClick)
import Validate exposing (Validator, Valid)
import String
import Http

import Session exposing (Session, Details)
import Routing exposing (Route(..))
import Session exposing (Session, PageType(..))
import Ports.FileUploadPort exposing (FilePortData, fileSelected, fileContentRead)
import UI.Elements as El

import Api.Types exposing (Gender(..), Image)
import Api.Authentication exposing (UserInfo)
import Api.Users exposing (User, EditUserDTO)



-- MODEL


type alias Model =
    { session             : Session
    , title               : String
    , loaded              : Bool
    , errors              : List (Error)
    , attemptedSubmission : Bool
    , password1           : (Bool, String)
    , password2           : (Bool, String)
    , gender              : (Bool, Gender)
    , birthday            : (Bool, String)
    , city                : (Bool, String)
    , bio                 : (Bool, String)
    , mImage              : Maybe Image
    }


type alias Error =
    ( FormField, String )


type FormField
    = Password1
    | Password2
    | Birthday
    | City
    | Bio


emptyModel : Session -> Model
emptyModel session =
     Model session "Edit profile" False [] False (False, "") (False, "") (False, Male) (False, "") (False, "") (False, "") Nothing

init : Session -> ( Model, Cmd Msg )
init session =
    case session of
        Session.Guest _ _ ->
            ( emptyModel session
            , Routing.goToLogin (Session.getNavKey session)
            )
        Session.LoggedIn _ _ userInfo ->
            ( updateErrors (emptyModel session)
            , sendGetUser HandleGetUser userInfo.username session
            )


-- UPDATE


type Msg
    = HandleGetUser (Result Http.Error User)
    | FormFieldChanged FormField String
    | GenderChanged Gender
    | FileSelected
    | FileRead FilePortData
    | Submitted
    | HandleUserUpdated (Result Http.Error UserInfo)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        HandleGetUser result ->
            case result of
                Ok { username, gender, birthday, town, profileText, image } ->
                    ( updateErrors { model | gender = (False, gender), birthday = (False, birthday), city = (False, town), bio = (False, profileText), loaded = True }, Cmd.none )
                Err errResponse ->
                    ( model
                    , Routing.replaceUrl (Session.getNavKey model.session) (Routing.routeToString Home )
                    )


        FormFieldChanged field value ->
            ( updateErrors <| setField model field value
            , Cmd.none
            )


        GenderChanged newGender ->
            ( { model | gender = (True, newGender) }, Cmd.none )


        FileSelected ->
            ( model
            , fileSelected ()
            )

        FileRead portData ->
            let
                image = Just (Image portData.contents portData.fileName)
            in
                if portData.error == "" then
                    ( { model | mImage = image}
                    , Cmd.none
                    )
                else
                    ( { model | mImage = Nothing, session = Session.addNotification model.session portData.error }
                    , Cmd.none
                    )


        Submitted ->
            case Validate.validate modelValidator model of
                Ok validForm ->
                    ( { model | errors = [] }
                    , sendEditUser HandleUserUpdated (userFromValidForm validForm) model.session
                    )
                Err errors ->
                    ( { model | errors = errors, attemptedSubmission = True }
                    , Cmd.none
                    )


        HandleUserUpdated result ->
            case result of
                Ok userInfo ->
                    ( model, Session.login userInfo)

                Err errResponse ->
                    case errResponse of
                        Http.BadUrl url ->
                            ( { model | session = Session.addNotification model.session ("Bad url: " ++ url) }, Cmd.none )

                        Http.BadPayload _ _ ->
                            ( { model | session = Session.addNotification model.session "Invalid data sent to server" }, Cmd.none )

                        Http.Timeout ->
                            ( { model | session = Session.addNotification model.session "Couldn't reach server" }, Cmd.none )

                        Http.NetworkError ->
                            ( { model | session = Session.addNotification model.session "Couldn't reach server" }, Cmd.none )

                        Http.BadStatus statusResponse ->
                            ( { model | session = Session.addNotification model.session ("Error: " ++ .body statusResponse) }, Cmd.none )



setField : Model -> FormField -> String -> Model
setField model field value =
    case field of
        Password1 ->
            { model | password1 = (value /= "", value) }
        Password2 ->
            { model | password2 = (value /= "", value) }
        Birthday ->
            { model | birthday = (True, value) }
        City ->
            { model | city = (True, value) }
        Bio ->
            { model | bio = (True, value) }


updateErrors : Model -> Model
updateErrors model =
    case Validate.validate modelValidator model of
        Ok validForm ->
            { model | errors = [] }
        Err errors ->
            { model | errors = errors }


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    fileContentRead FileRead


userFromValidForm : Valid Model -> EditUserDTO
userFromValidForm validForm =
    userFromModel (Validate.fromValid validForm)


userFromModel : Model -> EditUserDTO
userFromModel { password1, gender, birthday, city, bio, mImage } =
    { password      = (changedTupleToMaybe password1)
    , gender        = (changedTupleToMaybe gender)
    , birthday      = (changedTupleToMaybe birthday)
    , town          = (changedTupleToMaybe city)
    , profileText   = (changedTupleToMaybe bio)
    , image         = (changedTupleToMaybe (encodeMaybeImage mImage))
    }

changedTupleToMaybe : (Bool, dataType) -> Maybe dataType
changedTupleToMaybe (changed, data) =
    if changed then
        Just data
    else
        Nothing

encodeMaybeImage : Maybe Image -> (Bool, String)
encodeMaybeImage mImg =
    case mImg of
        Just img ->
         case List.head <| List.drop 1 (String.split "base64," img.contents) of
             Just data ->
                (True, data)
             Nothing ->
                (False, "")
        Nothing ->
            (False, "")


sendEditUser : (Result Http.Error (UserInfo) -> msg) -> EditUserDTO -> Session -> Cmd msg
sendEditUser responseMsg user session =
    case session of
        Session.LoggedIn _ _ userInfo ->
            Http.send responseMsg (Api.Users.postEditUser userInfo user)

        Session.Guest _ _ ->
            Cmd.none


sendGetUser : (Result Http.Error User -> msg) -> String -> Session -> Cmd msg
sendGetUser responseMsg username session =
    case session of
        Session.LoggedIn _ _ userInfo ->
            Http.send responseMsg (Api.Users.getUserByUsername userInfo username)

        Session.Guest _ _ ->
            Cmd.none


responseToString : Maybe String -> String
responseToString r =
    case r of
        Just msg ->
            msg

        Nothing ->
            "No messages"


-- VIEW

view : Model -> Session.Details Msg
view model =
    let
        password1 = Tuple.second model.password1
        password2 = Tuple.second model.password2
        city = Tuple.second model.city
        bio = Tuple.second model.bio
        birthday = Tuple.second model.birthday
        gender = Tuple.second model.gender
    in
        { title = model.title
        , session = model.session
        , kids = Scrollable
            <| El.titledContentLoader model.loaded model.title
                [ Html.form [ classList
                                [ ( "grid", True )
                                , ( "l-12", True )
                                ]
                            , Events.onSubmit Submitted
                            ]
                    [ El.validatedInput Password1 "password" "New password" password1 FormFieldChanged False model.errors model.attemptedSubmission
                    , El.validatedInput Password2 "password" "Repeat new password"  password2 FormFieldChanged False model.errors model.attemptedSubmission
                    , El.validatedInput City "text" "City" city FormFieldChanged False model.errors model.attemptedSubmission
                    , El.validatedInput Bio "text" "Description" bio FormFieldChanged False model.errors model.attemptedSubmission
                    , El.validatedInput Birthday "date" "Birthday" birthday FormFieldChanged False model.errors model.attemptedSubmission
                    , El.labelledRadio "Gender" GenderChanged gender
                        [ ( "Male", Male )
                        , ( "Female", Female )
                        , ( "Other", Other )
                        ]
                    , El.imageInput "Profile picture" FileSelected model.mImage
                    , El.submitButton "Update"
                    ]
                ]
        }


-- VALIDATION

modelValidator : Validator ( FormField, String ) Model
modelValidator =
    Validate.all
        [ Validate.ifFalse (\model -> doPasswordsMatch model) ( Password2, "Passwords don't match" )

        , Validate.ifTrue (\model -> Validate.isBlank <| Tuple.second model.birthday) ( Birthday, "Please enter your birthday" )

        , Validate.ifFalse (\model -> isDateValidFormat model ) ( Birthday, "Please enter birthday in a valid format" )
        , Validate.ifFalse (\model -> isDateValid model ) ( Birthday, "You must be at least 18 years old" )

        , Validate.ifTrue (\model -> Validate.isBlank <| Tuple.second model.city) ( City, "Please enter your city" )

        , Validate.ifTrue (\model -> Validate.isBlank <| Tuple.second model.bio) ( Bio, "Please write a short description" )
        ]


doPasswordsMatch : Model -> Bool
doPasswordsMatch model =
     model.password1 == model.password2


isDateValidFormat : Model -> Bool
isDateValidFormat model =
    let
        date = Tuple.second model.birthday
        dateLength = String.length date
        year = Maybe.withDefault -1 (String.toInt (String.slice 0 4 date))
        month = Maybe.withDefault -1 (String.toInt (String.slice 5 7 date))
        day = Maybe.withDefault -1 (String.toInt (String.slice 8 10 date))

        separators = String.slice 4 5 date ++ String.slice 7 8 date

        isYearNice = year >= 0 && year <= 9999
        isMonthNice = month >= 0 && month <= 12
        isDayNice = day >= 0 && day <= 31
        isSeparatorsNice = separators == "--"
    in
        dateLength == 10 && isYearNice && isSeparatorsNice && isMonthNice && isDayNice


isDateValid : Model -> Bool
isDateValid model =
    let
        date = Tuple.second model.birthday
        year = Maybe.withDefault -1 (String.toInt (String.slice 0 4 date))
        month = Maybe.withDefault -1 (String.toInt (String.slice 5 7 date))
        day = Maybe.withDefault -1 (String.toInt (String.slice 8 10 date))
    in
        year <= 2000
