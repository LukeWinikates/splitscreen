module Tests exposing (..)

import Test exposing (..)
import Expect
import Fuzz exposing (list, int, tuple, string)
import String
import Splitscreen.Model exposing (fromUrl, toUrl)


all : Test
all =
    describe "Model"
        [ describe "decoding"
            [ test "creating an object from the url hash fragment" <|
                \() ->
                    Expect.equal
                        { first = Just "http://jenkins-ci.org", second = Just "http://concourse.ci" }
                        (fromUrl "#?first=http%3A%2F%2Fjenkins-ci.org&second=http%3A%2F%2Fconcourse.ci")
            ]
        , describe "encoding"
            [ test "encoding the model as a url" <|
                \() ->
                    Expect.equal
                        "#?first=http%3A%2F%2Fjenkins-ci.org&second=http%3A%2F%2Fconcourse.ci"
                        (toUrl { first = Just "http://jenkins-ci.org", second = Just "http://concourse.ci" })
            ]
        ]
