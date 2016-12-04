module Tests exposing (..)

import Test exposing (..)
import Expect
import Fuzz exposing (list, int, tuple, string)
import String
import Splitscreen.Model exposing (fromUrl, toUrl)
import Dict exposing (..)


all : Test
all =
    describe "Model"
        [ describe "decoding"
            [ test "creating an object from the url hash fragment" <|
                \() ->
                    Expect.equal
                        { layout = [1, 1] , urls = fromList [ ( "x0y0", "http://jenkins-ci.org" ), ( "x1y0", "http://concourse.ci" ) ] }
                        (fromUrl "#?layout=11&x0y0=http%3A%2F%2Fjenkins-ci.org&x1y0=http%3A%2F%2Fconcourse.ci")
            ]
        , describe "encoding"
            [ test "encoding the model as a url" <|
                \() ->
                    Expect.equal
                        "#?layout=11&x0y0=http%3A%2F%2Fjenkins-ci.org&x1y0=http%3A%2F%2Fconcourse.ci"
                        (toUrl
                            { layout = [1 , 1]
                            , urls = fromList [ ( "x0y0", "http://jenkins-ci.org" ), ( "x1y0", "http://concourse.ci" ) ]
                            }
                        )
            ]
        ]
