module Common exposing (Msg(..), Notification)


type alias Notification = 
    { message: String
    }

-- TYPES
type Msg
    = NotificationReceived Notification
