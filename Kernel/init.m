(* ::Package:: *)

If[$VersionNumber < 10.2,
  Print["LesHouches requires Wolfram Language 10.2 or later."];
  Abort[]
]

(* register import converter if necessary *)
If[
  ! MemberQ[$ImportFormats, "LesHouches"], 
  Get[FileNameJoin @ {DirectoryName[$InputFileName, 2], "Formats", "LesHouches", "Import.m"}]
]

(* unprotect package symbols in case LesHouches` is double-loaded *)
Unprotect["LesHouches`*", "LesHouches`Convert`*", "LesHouches`Developer`*"];

(* load the package *)
Get["LesHouches`LesHouches`"]
Get["LesHouches`Convert`"]

(* protect all package symbols *)
SetAttributes[
  Flatten[
    Names /@ {"LesHouches`*", "LesHouches`Convert`*", "LesHouches`Developer`*"}
  ] // Evaluate,
  {Protected, ReadProtected}
]