module Messages exposing (Msg(..))

import Time

type Msg
    = NextPage
    | PrevPage
    | Tick Float
    | Update String Int String
    | Pause
    | StepF
    | StepB
    | AddPendulum
    | RemovePendulum
