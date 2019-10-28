(* ::Package:: *)

BeginPackage["CavityPlot`"]

Unprotect[cav2D, cav3D]


CavityPlot::usage = "This package is used to make a 3D plot as well as a 2D cross section plot of a cavity resonator. The functions should be called with a set of five parameters corresponding to 'Height', 'Gap size', 'Cone top radius', 'Cone bottom radius', 'Cavity radius', and 'Wall thickness'\n
cav2D : Plot the cross section of the cavity resonator with a set of five parameters.\n
cav3D : Plot in 3D the cavity resonator with a set of five parameters.\n"

Begin["CavityPlot`Private`"]
cav2D[{H0_,delta0_,r0_,rr0_,R0_,thick0_}]:= Module[{H = H0,delta = delta0,r = r0,rr = rr0,R = R0,
thick = thick0, theta = ArcTan[(H0 - delta0)/(rr0 - r0)]}, rCross = Line[{{-R,H},{R,H},{R,0},{rr,0},{r,H-delta},{-r,H-delta},{-rr,0},{-R,0},
{-R,H}}]; Graphics[{Gray,Tooltip[FilledCurve[rCross],"Cavity "],Tooltip[ Line[{{-R,H},{R,H}}],"Ceiline "],
Tooltip[ Line[{{-R,H},{-R,0}}],"Wall "],Tooltip[ Line[{{R,H},{R,0}}],"Wall "],{Opacity[0],Tooltip[Polygon[{{-rr,0},
{-r,H-delta},{r,H-delta},{rr,0}}],"Central Cone "]},Tooltip[ Line[{{-R,0},{-rr,0}}],"Bottom "],Tooltip[ Line[{{R,0},
{rr,0}}],"Bottom "]}, PlotLabel -> "Cross section of the cavity "]];

cav3D [{H0_,delta0_,r0_,rr0_,R0_,thick0_}]:= Module[{H = H0,delta = delta0,r = r0,rr = rr0,R = R0,
thick = thick0, theta = ArcTan[(H0 - delta0)/(rr0 - r0)]}, func2[x_]:=Piecewise[{{H-delta,0<=x<=r},{-Tan[theta](x-r)+H-delta,r<x<rr},
{0,rr<=x<=R}}];p2 = RevolutionPlot3D[func2[x],{x,0,R},Mesh->None];
cyl1 = DiscretizeRegion[Cylinder[{{0,0,0},
{0,0,H}},R+thick]];
cyl2 = DiscretizeRegion[Cylinder[{{0,0,0},{0,0,H}},R]];
tube = RegionDifference[cyl1,cyl2 ];Show[p2,tube,BoundaryStyle -> Automatic, Mesh->None ]]
End[]

Protect[cav2D, cav3D]
EndPackage[]


Print["CavityPlot package loaded"]
