module Issues exposing (all)

import Test exposing (describe, test, Test)
import Expect exposing (..)

import Ast exposing (parseStatement, parse)
import Ast.BinOp exposing (Assoc(..), operators)
import Ast.Expression exposing (Expression(..))
import Ast.Statement exposing (ExportSet(..), Type(..), Statement(..))

is : String -> Statement -> Expectation
is i s =
  case parseStatement operators i of
    (Ok r, _) ->
      Expect.equal r s

    _ ->
      Expect.fail ("not" ++ i)

are : String -> List Statement -> Expectation
are i s =
  case parse i of
    (Ok r, _) ->
      Expect.equal r s

    _ ->
      Expect.fail ("not" ++ i)

issueN1InputOneliner : String
issueN1InputOneliner = """
import Html exposing (..)

main =
  div [] [ text "Hello, World!" ]
"""

issueN1InputMultiliner : String
issueN1InputMultiliner = """
import Html exposing (..)

main =
  div []
  [ text "Hello, World!" ]
"""

issueN1ExpectedStatements: List Statement
issueN1ExpectedStatements = [ ImportStatement ["Html"] Nothing (Just AllExport)
                            , FunctionDeclaration "main" []
                                (Application
                                    (Application
                                        (Variable ["div"])
                                        (List [])
                                    )
                                    (List
                                        ([Application (Variable ["text"])
                                                      (String "Hello, World!")])
                                    )
                                )
                            ]

issueN1 : Test
issueN1 =
  describe "Issue #1"
    [ test "one-liner version" <|
        \() -> issueN1InputOneliner `are` issueN1ExpectedStatements
    , test "multi-liner version" <|
        \() -> issueN1InputMultiliner `are` issueN1ExpectedStatements
    ]

all : Test
all =
  describe "Issues suite"
    [ issueN1
    ]
