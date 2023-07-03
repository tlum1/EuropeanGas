(* ::Package:: *)

BeginPackage["WorkingWithData`"]


getExportData::usage = "getExportData[] \:0412\:043e\:0437\:0432\:0440\:0430\:0449\:0430\:0435\:0442 \:0442\:0430\:0431\:043b\:0438\:0446\:0443 \:044d\:043a\:0441\:043f\:043e\:0440\:0442\:0430 \:0433\:0430\:0437\:0430";
getImportData::usage= "getImportData[] ";
castToM3::usage = "castToM3[data] \:041f\:0435\:0440\:0435\:0432\:043e\:0434\:0438\:0442 \:0434\:0430\:043d\:043d\:044b\:0435 \:0438\:0437 \:0413\:0438\:0433\:0430\:0432\:0430\:0442\:0442/\:0447\:0430\:0441 \:0432 \!\(\*SuperscriptBox[\(\:043c\), \(3\)]\)";
castToGWh::usage="castToGWh[data] \:041f\:0435\:0440\:0435\:0432\:043e\:0434\:0438\:0442 \:0434\:0430\:043d\:043d\:044b\:0435 \:0438\:0437 \!\(\*SuperscriptBox[\(\:043c\), \(3\)]\) \:0432 \:0413\:0438\:0433\:0430\:0432\:0430\:0442\:0442/\:0447\:0430\:0441";
getAGSIData::usage = "getAGSIData[] \:0412\:043e\:0437\:0432\:0440\:0430\:0449\:0430\:0435\:0442 \:0430\:0433\:0440\:0435\:0433\:0438\:0440\:043e\:0432\:0430\:043d\:043d\:0443\:044e \:0442\:0430\:0431\:043b\:0438\:0446\:0443 \:0441 \:0434\:0430\:043d\:043d\:044b\:043c\:0438 AGSI";
getALSIData::usage = "getALSIData[] \:0412\:043e\:0437\:0432\:0440\:0430\:0449\:0430\:0435\:0442 \:0430\:0433\:0440\:0435\:0433\:0438\:0440\:043e\:0432\:0430\:043d\:043d\:0443\:044e \:0442\:0430\:0431\:043b\:0438\:0446\:0443 \:0441 \:0434\:0430\:043d\:043d\:044b\:043c\:0438 ALSI";
getDataByMonth::usage = "getDataByMonth[data, date] \:0412\:043e\:0437\:0440\:0430\:0449\:0430\:0435\:0442 \:0434\:0430\:043d\:043d\:044b\:0435 \:0437\:0430 \:043c\:0435\:0441\:044f\:0446 \:043f\:043e \:043e\:0434\:043d\:043e\:0439 \:0438\:0437 \:0442\:0430\:0431\:043b\:0438\:0446 (AGSI, ALSI, Export, Import, Balance)";
getDataByYear::usage = "getDataByYear[data, year] \:0412\:043e\:0437\:0440\:0430\:0449\:0430\:0435\:0442 \:0434\:0430\:043d\:043d\:044b\:0435 \:0437\:0430 \:0433\:043e\:0434 \:043f\:043e \:043e\:0434\:043d\:043e\:0439 \:0438\:0437 \:0442\:0430\:0431\:043b\:0438\:0446 (AGSI, ALSI, Export, Import, Balance)";
getDataByQuarter::usage = "getDataByQuarter[data, year, quarter] \:0412\:043e\:0437\:0440\:0430\:0449\:0430\:0435\:0442 \:0434\:0430\:043d\:043d\:044b\:0435 \:0437\:0430 \:043a\:0432\:0430\:0440\:0442\:0430\:043b \:043f\:043e \:043e\:0434\:043d\:043e\:0439 \:0438\:0437 \:0442\:0430\:0431\:043b\:0438\:0446 (AGSI, ALSI, Export, Import, Balance)";
aggregate::usage = "aggregate[data, func] \:0412\:043e\:0437\:0440\:0430\:0449\:0430\:0435\:0442 \:0430\:0433\:0440\:0435\:0433\:0438\:0440\:043e\:0432\:0430\:043d\:043d\:044b\:0435 \:0434\:0430\:043d\:043d\:044b\:0435 \:043f\:043e \:043f\:0435\:0440\:0435\:0434\:0430\:043d\:043d\:043e\:0439 \:0444\:0443\:043d\:043a\:0446\:0438\:0438";
getGasFlowVisualisation::usage="getGasFlowVisualisation[data, units:'GWh'] \:0432\:043e\:0437\:0432\:0440\:0430\:0449\:0430\:0435\:0442 \:0433\:0440\:0430\:0444 \:0441 \:043f\:043e\:0442\:043e\:043a\:0430\:043c\:0438 \:0433\:0430\:0437\:0430 \:043c\:0435\:0436\:0434\:0443 \:0441\:0442\:0440\:0430\:043d\:0430\:043c\:0438";


Begin["`Private`"]


(* ::Section:: *)
(*\:041e\:0431\:0449\:0438\:0435 \:043a\:043e\:0434\:044b \:043e\:0448\:0438\:0431\:043e\:043a*)


data::invalidArgs = "\:0412\:0432\:0435\:0434\:0435\:043d\:044b \:043d\:0435\:0432\:0435\:0440\:043d\:044b\:0435 \:043f\:0430\:0440\:0430\:043c\:0435\:0442\:0440\:044b";
getData::invalidYear = "\:0413\:043e\:0434 \:0434\:043e\:043b\:0436\:0435\:043d \:0431\:044b\:0442\:044c \:0432 \:043f\:0440\:0435\:0434\:0435\:043b\:0435 \:043e\:0442 2018 \:0434\:043e 2022";
getData::invalidQuarter = "\:041a\:0432\:0430\:0440\:0442\:0430\:043b \:0434\:043e\:043b\:0436\:0435\:043d \:0431\:044b\:0442\:044c \:0432 \:043f\:0440\:0435\:0434\:0435\:043b\:0435 \:043e\:0442 1 \:0434\:043e 4";


(* ::Section:: *)
(*getBalanceData \:0432\:044b\:0434\:0430\:0435\:0442 \:0434\:0430\:043d\:043d\:044b\:0435 \:0431\:0430\:043b\:0430\:043d\:0441\:043e\:0432\:043e\:0439 \:0442\:0430\:0431\:043b\:0438\:0446\:044b \:0432 \:0413\:0438\:0433\:0430\:0432\:0430\:0442\:0442/\:0447\:0430\:0441*)


getBalanceData::nofile = "File 'BALANCE' was not found during import";

getBalanceData[] :=
    Module[{result},
        result = Quiet @ Get["BALANCE"];
        If[result === $Failed,
            Message[getBalanceData::nofile]; $Failed
            ,
            Delete[result[[4 ;; ]], "EL"]
        ]
    ]

getBalanceData[args__] :=
    Message[data::invalidArgs]


(* ::Section:: *)
(*getImportData, getExportData \:0432\:044b\:0434\:0430\:044e\:0442 \:0434\:0430\:043d\:043d\:044b\:0435 \:044d\:043a\:0441\:043f\:043e\:0440\:0442\:0430 \:0438 \:0438\:043c\:043f\:043e\:0440\:0442\:0430 \:0432 \:0413\:0438\:0433\:0430\:0432\:0430\:0442\:0442/\:0447\:0430\:0441*)


getExportData::nofile = "File 'exportGWh' was not found during import";

getImportData::nofile = "File 'importGWh' was not found during import";

getExportData[] :=
    Module[{data},
        data = Quiet @ Get["exportGWh"];
        If[data === $Failed,
            Message[getExportData::nofile]; $Failed,
            data = data[[1]]; data = Delete[data, {{Key["EA"]}, {Key["EU27_2020"
                ]}, {Key["EA19"]}, {Key["EL"]}, {Key["EX_SU_OTH"]}}]; data = Map[Delete[
                #, {{Key["EL"]}, {Key["NSP"]}}]&, data]
        ]
    ]

getExportData[args__] :=
    Message[data::invalidArgs]

getImportData[] :=
    Module[{data},
        data = Quiet @ Get["importGWh"];
        If[data === $Failed,
            Message[getImportData::nofile]; $Failed,
            data = data[[1]]; data = Delete[data, {{Key["EA"]}, {Key["EU27_2020"
                ]}, {Key["EA19"]}, {Key["EL"]}, {Key["EX_SU_OTH"]}}]; data = Map[Delete[
                #, {{Key["EL"]}, {Key["NSP"]}}]&, data]
        ]
    ]

getImportData[args__] :=
    Message[data::invalidArgs]


(* ::Section:: *)
(*\:041f\:0435\:0440\:0435\:0432\:043e\:0434 \:0432 \:0434\:0440\:0443\:0433\:0438\:0435 \:0415\:0434. \:0438\:0437\:043c\:0435\:0440\:0435\:043d\:0438\:044f*)


castToM3[data_Association]:=Map[#/0.01029&, data]
castToM3[args___]:=Message[data::invalidArgs]
castToGWh[data_Association]:=Map[#&, data]

castToGWh[args___]:=Message[data::invalidArgs]


(* ::Section:: *)
(*getAGSIData \:0432\:044b\:0434\:0430\:0435\:0442 \:0434\:0430\:043d\:043d\:044b\:0435 \:0441 \:0441\:0430\:0439\:0442\:0430 AGSI \:0432 \:0413\:0438\:0433\:0430\:0432\:0430\:0442\:0442/\:0447\:0430\:0441*)
(*getALSIData \:0432\:044b\:0434\:0430\:0435\:0442 \:0434\:0430\:043d\:043d\:044b\:0435 \:0441 \:0441\:0430\:0439\:0442\:0430 ALSI \:0432 \:0413\:0438\:0433\:0430\:0432\:0430\:0442\:0442/\:0447\:0430\:0441*)


getAGSIData::nofile = "File 'AGSI_aggregated' was not found during import";

getALSIData::nofile = "File 'ALSI_aggregated' was not found during import"

getAGSIData[] :=
    Module[{data},
        data = Quiet @ Get["AGSI_aggregated"];
        If[data === $Failed,
            Message[getAGSIData::nofile]; $Failed
            ,
            data
        ]
    ]

getAGSIData[args__] :=
    Message[data::invalidArgs]

getALSIData[] :=
    Module[{data},
        data = Quiet @ Get["ALSI_aggregated"];
        If[data === $Failed,
            Message[getALSIData::nofile]; $Failed
            ,
            data
        ]
    ]


getALSIData[args__] :=
    Message[data::invalidArgs]


(* ::Section:: *)
(*getDataByMonth \:0432\:044b\:0434\:0430\:0435\:0442 \:0434\:0430\:043d\:043d\:044b\:0435 \:0437\:0430 \:043c\:0435\:0441\:044f\:0446 \:0432 \:0413\:0438\:0433\:0430\:0432\:0430\:0442\:0442/\:0447\:0430\:0441*)
(*getDataByYear \:0432\:044b\:0434\:0430\:0435\:0442 \:0434\:0430\:043d\:043d\:044b\:0435 \:0437\:0430 \:0433\:043e\:0434 \:0432 \:0413\:0438\:0433\:0430\:0432\:0430\:0442\:0442/\:0447\:0430\:0441*)
(*getDataByQuarter \:0432\:044b\:0434\:0430\:0435\:0442 \:0434\:0430\:043d\:043d\:044b\:0435 \:0437\:0430 \:043a\:0432\:0430\:0440\:0442\:0430\:043b*)


getDataByMonth[data_Association, date_String] :=
    If[Length @ Dimensions @ data == 2,
        data[[All, date]]
        ,
        Map[#[date]&, data, {2}]
    ]
getDataByMonth[args___]:=Message[data::invalidArgs]


getDataByYear[data_Association, year_String] /; 2018 <= ToExpression @
     year <= 2021 :=
    If[Length @ Dimensions @ data == 2,
        Map[#[[Normal @ Select[Keys @ (#), StringContainsQ[#, year]&]
            ]]&, data]
        ,
        Map[#[[Normal @ Select[Keys @ (#), StringContainsQ[#, year]&]
            ]]&, data, {2}]
    ]

getDataByYear[data_Association, year_String] /; \[Not](2018 <= ToExpression
     @ year <= 2021) :=
    Message[getData::invalidYear]

getDataByYear[args___] :=
    Message[data::invalidArgs]


getDataByQuarter[data_Association, year_String, quarter_Integer] /; 1 <= quarter <= 4 \[And] 2018 <= ToExpression @
     year <= 2021:=
    If[Length @ Dimensions @ data == 2,
        (Reverse @ getDataByYear[data, year])[[(3 * quarter - 2) ;; (
            3 * quarter)]]
        ,
        Map[
            If[Length @ # == 0,
                <||>
                ,
                (Reverse @ #)[[(3 * quarter - 2) ;; (3 * quarter)]]
            ]&
            ,
            getDataByYear[data, year]
            ,
            {2}
        ]
    ]
getDataByQuarter[data_Association, year_String, quarter_Integer]/;\[Not](2018<= ToExpression@year<=2021):=Message[getData::invalidYear]
getDataByQuarter[data_Association, year_String, quarter_Integer]/;\[Not](1 <= quarter <= 4) :=Message[getData::invalidQuarter]
getDataByQuarter[args___]:=Message[data::invalidArgs]


(* ::Section:: *)
(*aggreagte - \:0430\:0433\:0440\:0435\:0433\:0438\:0440\:0443\:0435\:0442 \:0434\:0430\:043d\:043d\:044b\:0435 \:043f\:043e \:043f\:0435\:0440\:0435\:0434\:0430\:043d\:043d\:043e\:0439 \:0444\:0443\:043d\:043a\:0446\:0438\:0438*)


aggregate[data_Association, func_] :=
    If[Length @ Dimensions @ data == 2,
        Map[func @ Values @ #&, data]
        ,
        Map[func @ Values @ #&, data, {2}]
    ]
aggregate[args___]:=Message[data::invalidArgs]


(* ::Section:: *)
(*getFlowVisualisation - \:0432\:043e\:0437\:0432\:0440\:0430\:0449\:0430\:0435\:0442 \:0433\:0440\:0430\:0444 \:0441 \:043f\:043e\:0442\:043e\:043a\:0430\:043c\:0438 \:0433\:0430\:0437\:0430 \:043c\:0435\:0436\:0434\:0443 \:0441\:0442\:0440\:0430\:043d\:0430\:043c\:0438*)
(*getGasFlowVisualisation - \:043f\:0440\:043e\:043c\:0435\:0436\:0443\:0442\:043e\:0447\:043d\:0430\:044f \:0444\:0443\:043d\:043a\:0446\:0438\:044f \:043c\:0435\:0436\:0434\:0443 \:0444\:0443\:043d\:043a\:0446\:0438\:0435\:0439 getFlowVisualisation*)
(*getBestCountries - \:0441\:043e\:0441\:0442\:0430\:0432\:043b\:0435\:043d\:0438\:0435 \:0442\:0430\:0431\:043b\:0438\:0446\:044b \:0441 \:043b\:0443\:0447\:0448\:0438\:043c\:0438 \:0441\:0442\:0440\:0430\:043d\:0430\:043c\:0438*)


getBestCountries[data_] :=
    Module[{sorted = (ReverseSort @ Map[Total @ #&, data /. x_Missing
         -> 0])[[ ;; 3]]},
        Labeled[Grid[{Interpreter["Country"][#]& /@ Keys @ sorted, Values 
            @ sorted}, Frame -> All, ItemStyle->Directive[FontSize -> 28],Spacings -> {2, 2}], "First three countries\nwith largest gas flow",
             Top, LabelStyle -> Directive[Bold, 24]]
    ]


getFlowVisualisation[data_Association,type_, units_String]:=Module[
{
countryPairs,
 weights,
deletePos,
countryRules = If[StringContainsQ[type, "\:0438\:043c\:043f"], {"Name", Entity["City",{"Moscow","Moscow","Russia"}]->Entity["Country","Russia"], Entity["City",{"Tripoli","Tripoli","Libya"}]->Entity["Country","Libya"], Entity["City",{"Tabriz","EastAzerbaijan","Iran"}]->Entity["Country","Iran"]}, {"Name", Entity["City",{"Moscow","Moscow","Russia"}]->Entity["Country","Russia"]}]
},
countryPairs = Flatten@Map[Riffle[Keys@data[#], #,{1,-2,2}]&,Keys@data];
     countryPairs =Partition[countryPairs,2];
     countryPairs = Map[Apply[DirectedEdge,{Interpreter["Country"][#[[2]]], Interpreter["Country"][#[[1]]]}]&,countryPairs]/.{Entity["Country","Belarus"]->Entity["City",{"Moscow","Moscow","Russia"}],Entity["Country","Russia"]->Entity["City",{"Moscow","Moscow","Russia"}], Entity["Country","Libya"]->Entity["City",{"Tripoli","Tripoli","Libya"}], Entity["Country","Iran"]->Entity["City",{"Tabriz","EastAzerbaijan","Iran"}]};
deletePos = Position[countryPairs,  Entity["Country","PapuaNewGuinea"]\[DirectedEdge]x_] ~Join~Position[countryPairs,  Entity["Country","UnitedStates"]\[DirectedEdge]x_]~Join~Position[countryPairs,  Entity["Country","Qatar"]\[DirectedEdge]x_];

AppendTo[countryPairs, Entity["City",{"Moscow","Moscow","Russia"}]\[DirectedEdge]Entity["Country","Ukraine"]];
     weights = Map[#&,Flatten@Values@Values@data]/.Missing[x__]->0;
AppendTo[weights, Total@Extract[weights,Position[countryPairs, Entity["Country","Ukraine"]\[DirectedEdge]x_]]];
weights=Delete[weights, deletePos];
countryPairs = Delete[countryPairs, deletePos];
GraphicsRow[
{
GeoGraphValuePlot[Graph[countryPairs, EdgeWeight->weights],
Method->{"MinDisplayedEdgeValue"->0}, 
GeoRange->{GeoPosition[Entity["Country","Finland"]], GeoPosition[Entity["Country","Algeria"]]},
GeoCenter->Automatic,
MinPointSeparation->0, 
EdgeValueSizes->{0.002,0.003}, MinPointSeparation->0,
PlotTheme->"Minimal", VertexSize->Large,
VertexLabels->countryRules,
VertexLabelStyle->Directive[FontSize->16],
ImageSize->{1024, 768}, 
PlotLabel->Style["Gas flow",Bold,24,
FontFamily->"Arial"], Background->White],
Grid[
 {{SwatchLegend["SiennaTones",Reverse@DeleteDuplicates[Round[#, 10000]&/@weights], LegendLabel->units, LabelStyle->{FontSize->24, Bold}, LegendMarkerSize->{24, 24}]},
 {getBestCountries[data]}}
 ]
},
 ImageSize->{1920, 1080}]
]


getGasFlowVisualisation[data_Association, type_String:"\:0438\:043c\:043f\:043e\:0440\:0442", units_String
    :"GWh"]/;type=="\:0438\:043c\:043f\:043e\:0440\:0442"\[Or]type=="\:044d\:043a\:0441\:043f\:043e\:0440\:0442" :=
    getFlowVisualisation[data, type, units]
getGasFlowVisualisation[args___]:=Message[getData::invalidArgs]


End[]


EndPackage[]
