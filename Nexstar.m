(* ::Package:: *)

BeginPackage["Nexstar`"]

ScopeConnect::usage = "ScopeConnect[] or ScopeConnect[device] opens a connection to the scope"
Slew::usage = "Slew[scope, ra, dec] slews to the RA or DEC specified. RA and DEC can be Reals or multi-radixs as returned from the GalaxyData, etc functions."
SlewTo::usage = "SlewTo[scope, type, name] slews to an object type by name. types can be galaxy, star, nebula or cluster"


Begin["`Private`"]


ScopeConnect[device_] := DeviceOpen["Serial", {"/dev/ttyUSB0","BaudRate" -> 9600}]
ScopeConnect[] := ScopeConnect["/dev/ttyUSB0"]


(* The Head of the mixed-radix values returned by GalaxyData, StarData, etc is Quantity - there's probably a better way to do this *)
RA2Hex[ra_Real] := ToUpperCase[IntegerString[Round[(ra / 360) * 65536], 16, 4]]
RA2Hex[ra_Quantity] := RA2Hex[FromDMS[ra]]
Declination2Hex[dec_Real] := ToUpperCase[IntegerString[Round[(If[dec < 0, 360 + dec, dec]/360) * 65536], 16, 4]]
Declination2Hex[dec_Quantity] := Declination2Hex[FromDMS[dec]]


SlewString[ra_,dec_]:="R"<>RA2Hex[ra]<>","<>Declination2Hex[dec]


Slew[scope_, ra_, dec_] := Module[{},
  DeviceWrite[scope, SlewString[ra, dec]];
  <| SlewedTo -> {ra, dec}, 
     At -> DateString[] |>
]


SlewTo[scope_, "galaxy", name_] := Slew[scope, #1, #2] & @@ GalaxyData[name, {"RightAscension", "Declination"}]
SlewTo[scope_, "star", name_] := Slew[scope, #1, #2] & @@ StarData[name, {"RightAscension", "Declination"}]
SlewTo[scope_, "nebula", name_] := Slew[scope, #1, #2] & @@ NebulaData[name, {"RightAscension", "Declination"}]
SlewTo[scope_, "cluster", name_] := Slew[scope, #1, #2] & @@ StarClusterData[name, {"RightAscension", "Declination"}]



End[]
EndPackage[]
