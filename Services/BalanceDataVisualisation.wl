(* ::Package:: *)

BeginPackage["BalanceDataVisualisation`"]


getMeetingDemandByYear::usage = "getMeetingDemandByYear[country, year] makes TabView of piecharts shows meeting gas demand by different time segments";

getComparativeByYear::usage = "getMeetingDemandByYear[country, year] makes TabView of barcharts compare gas parameters by different time segments";

getGeoParamVisualisationByYear::usage = "getGeoParamVisualisationByYearp[year] makes TabView of maps showed each of param visualisation";

getBalanceData::usage = "getBalanceData[] retrurns gas balance table";

exportToXlsx::usage = "exportToXlsx[country] \:042d\:043a\:0441\:043f\:043e\:0440\:0442\:0438\:0440\:0443\:0435\:0442 \:0434\:0430\:043d\:043d\:044b\:0435 \:043f\:043e \:0441\:0442\:0440\:0430\:043d\:0435 \:0438\:0437 \:0431\:0430\:043b\:0430\:043d\:0441\:0432\:043e\:0439 \:0442\:0430\:0431\:043b\:0438\:0446\:044b \:0432 xslx";

exportAllToXlsx::usage = "exportToAllXlsx[] \:042d\:043a\:0441\:043f\:043e\:0440\:0442\:0438\:0440\:0443\:0435\:0442 \:0434\:0430\:043d\:043d\:044b\:0435 \:043f\:043e \:0441\:0442\:0440\:0430\:043d\:0435 \:0438\:0437 \:0431\:0430\:043b\:0430\:043d\:0441\:0432\:043e\:0439 \:0442\:0430\:0431\:043b\:0438\:0446\:044b \:0432 xslx";


Begin["`Private`"]


(* ::Section:: *)
(*\:0424\:0443\:043d\:043a\:0446\:0438\:044f \:0434\:043b\:044f \:043f\:0440\:043e\:0432\:0435\:0440\:043a\:0438 \:043d\:0430\:043b\:0438\:0447\:0438\:044f \:043a\:043e\:0434\:0430 \:0441\:0442\:0440\:0430\:043d\:044b \:0432 \:043a\:043b\:044e\:0447\:0430\:0445 \:0431\:0430\:043b\:0430\:043d\:0441\:043e\:0432\:043e\:0439 \:0442\:0430\:0431\:043b\:0438\:0446\:044b*)


balanceKeysMemberQ[key_String] := MemberQ[Keys @ getBalanceData[], key]


(* ::Section:: *)
(*getBalanceData \:0432\:044b\:0434\:0430\:0435\:0442 \:0434\:0430\:043d\:043d\:044b\:0435 \:0431\:0430\:043b\:0430\:043d\:0441\:043e\:0432\:043e\:0439 \:0442\:0430\:0431\:043b\:0438\:0446\:044b \:0432 \:0413\:0438\:0433\:0430\:0432\:0430\:0442\:0442/\:0447\:0430\:0441*)


getBalanceData::nofile = "File 'BALANCE' was not found during import";

getBalanceData[] :=
    Module[{result},
        result = Quiet @ Get["BALANCE"];
        If[result === $Failed,
            Message[getBalanceData::nofile]; $Failed,
            Delete[result[[4 ;; ]], "EL"]
        ]
    ]


(* ::Section:: *)
(*comparativeVisualisationByMonth  - \:0421\:043e\:0441\:0442\:0430\:0432\:043b\:0435\:043d\:0438\:044f \:0433\:0438\:0441\:0442\:043e\:0433\:0440\:0430\:043c\:043c\:044b, \:0441\:043e\:043f\:043e\:0441\:0442\:0430\:0432\:043b\:044f\:044e\:0449\:0435\:0439 \:0434\:0430\:043d\:043d\:044b\:0435 \:0442\:0430\:043a\:0438\:0435 \:043a\:0430\:043a, \:0438\:043c\:043f\:043e\:0440\:0442-\:044d\:043a\:0441\:043f\:043e\:0440\:0442, \:0434\:043e\:0431\:044b\:0447\:0430-\:043f\:043e\:0442\:0440\:0435\:0431\:043b\:0435\:043d\:0438\:0435, \:0437\:0430\:043a\:0430\:0447\:043a\:0430 \:0432 \:041f\:0425\:0413-\:0438\:0437\:044a\:044f\:0442\:0438\:0435 \:0438\:0437 \:041f\:0425\:0413 \:0434\:043b\:044f \:043e\:043f\:0440\:0435\:0434\:0435\:043b\:0435\:043d\:043d\:043e\:0439 \:0441\:0442\:0440\:0430\:043d\:044b \:0432 \:043e\:043f\:0440\:0435\:0434\:0435\:043b\:0435\:043d\:043d\:044b\:0439 \:043c\:0435\:0441\:044f\:0446*)
(*comparativeVisualisationByQuarter - \:0421\:043e\:0441\:0442\:0430\:0432\:043b\:0435\:043d\:0438\:044f \:0433\:0438\:0441\:0442\:043e\:0433\:0440\:0430\:043c\:043c\:044b, \:0441\:043e\:043f\:043e\:0441\:0442\:0430\:0432\:043b\:044f\:044e\:0449\:0435\:0439 \:0434\:0430\:043d\:043d\:044b\:0435 \:0442\:0430\:043a\:0438\:0435 \:043a\:0430\:043a, \:0438\:043c\:043f\:043e\:0440\:0442-\:044d\:043a\:0441\:043f\:043e\:0440\:0442, \:0434\:043e\:0431\:044b\:0447\:0430-\:043f\:043e\:0442\:0440\:0435\:0431\:043b\:0435\:043d\:0438\:0435, \:0437\:0430\:043a\:0430\:0447\:043a\:0430 \:0432 \:041f\:0425\:0413-\:0438\:0437\:044a\:044f\:0442\:0438\:0435 \:0438\:0437 \:041f\:0425\:0413 \:0434\:043b\:044f \:043e\:043f\:0440\:0435\:0434\:0435\:043b\:0435\:043d\:043d\:043e\:0439 \:0441\:0442\:0440\:0430\:043d\:044b \:0432 \:043e\:043f\:0440\:0435\:0434\:0435\:043b\:0435\:043d\:043d\:044b\:0439 \:043a\:0432\:0430\:0440\:0442\:0430\:043b (1-4) \:0432 \:043e\:043f\:0440\:0435\:0434\:0435\:043b\:0435\:043d\:043d\:044b\:0439 \:0433\:043e\:0434*)
(*comparativeVisualisationByYear - \:0421\:043e\:0441\:0442\:0430\:0432\:043b\:0435\:043d\:0438\:044f \:0433\:0438\:0441\:0442\:043e\:0433\:0440\:0430\:043c\:043c\:044b, \:0441\:043e\:043f\:043e\:0441\:0442\:0430\:0432\:043b\:044f\:044e\:0449\:0435\:0439 \:0434\:0430\:043d\:043d\:044b\:0435 \:0442\:0430\:043a\:0438\:0435 \:043a\:0430\:043a, \:0438\:043c\:043f\:043e\:0440\:0442-\:044d\:043a\:0441\:043f\:043e\:0440\:0442, \:0434\:043e\:0431\:044b\:0447\:0430-\:043f\:043e\:0442\:0440\:0435\:0431\:043b\:0435\:043d\:0438\:0435, \:0437\:0430\:043a\:0430\:0447\:043a\:0430 \:0432 \:041f\:0425\:0413-\:0438\:0437\:044a\:044f\:0442\:0438\:0435 \:0438\:0437 \:041f\:0425\:0413 \:0434\:043b\:044f \:043e\:043f\:0440\:0435\:0434\:0435\:043b\:0435\:043d\:043d\:043e\:0439 \:0441\:0442\:0440\:0430\:043d\:044b \:0432 \:043e\:043f\:0440\:0435\:0434\:0435\:043b\:0435\:043d\:043d\:044b\:0439 \:0433\:043e\:0434*)


comparativeVisualisationByMonth[country_, date_] :=
    comparativeVisualisation[getBalanceDataByMonth[country, date], date
        ]

comparativeVisualisationByQuarter[country_, year_, quarter_] /; 1 <= quarter <=
     4 \[And] 2018 <= ToExpression @ year <= 2021 :=
    comparativeVisualisation[getBalanceDataByQuarter[country, year, quarter
        ], ToString @ year <> " " <> ToString @ quarter <> " \:043a\:0432\:0430\:0440\:0442\:0430\:043b"]

comparativeVisualisationByYear[country_, year_] /; 2018 <= ToExpression @
     year <= 2021 :=
    comparativeVisualisation[aggregate @ getBalanceDataByYear[country,
         year], year]


(* ::Section:: *)
(*getMeetingDemandVisualisationByMonth -  \:0421\:043e\:0441\:0442\:0430\:0432\:043b\:0435\:043d\:0438\:0435 \:043a\:0440\:0443\:0433\:043e\:0432\:043e\:0439 \:0434\:0438\:0430\:0433\:0440\:0430\:043c\:043c\:044b, \:043e\:0442\:0440\:0430\:0436\:0430\:044e\:0449\:0435\:0439 \:0437\:0430 \:0441\:0447\:0435\:0442 \:0447\:0435\:0433\:043e \:043f\:0440\:043e\:0438\:0441\:0445\:043e\:0434\:0438\:0442 \:0443\:0434\:043e\:0432\:043b\:0435\:0442\:0432\:043e\:0440\:0435\:043d\:0438\:044f \:043f\:043e\:0442\:0440\:0435\:0431\:043d\:043e\:0441\:0442\:0438 \:0432 \:0433\:0430\:0437\:0435 \:0434\:043b\:044f \:043e\:043f\:0440\:0435\:0434\:0435\:043b\:0435\:043d\:043d\:043e\:0439 \:0441\:0442\:0440\:0430\:043d\:044b \:0432 \:043e\:043f\:0440\:0435\:0434\:0435\:043b\:0435\:043d\:043d\:044b\:0439 \:043c\:0435\:0441\:044f\:0446*)
(*getMeetingDemandVisualisationByYear -  \:0421\:043e\:0441\:0442\:0430\:0432\:043b\:0435\:043d\:0438\:0435 \:043a\:0440\:0443\:0433\:043e\:0432\:043e\:0439 \:0434\:0438\:0430\:0433\:0440\:0430\:043c\:043c\:044b, \:043e\:0442\:0440\:0430\:0436\:0430\:044e\:0449\:0435\:0439 \:0437\:0430 \:0441\:0447\:0435\:0442 \:0447\:0435\:0433\:043e \:043f\:0440\:043e\:0438\:0441\:0445\:043e\:0434\:0438\:0442 \:0443\:0434\:043e\:0432\:043b\:0435\:0442\:0432\:043e\:0440\:0435\:043d\:0438\:044f \:043f\:043e\:0442\:0440\:0435\:0431\:043d\:043e\:0441\:0442\:0438 \:0432 \:0433\:0430\:0437\:0435 \:0434\:043b\:044f \:043e\:043f\:0440\:0435\:0434\:0435\:043b\:0435\:043d\:043d\:043e\:0439 \:0441\:0442\:0440\:0430\:043d\:044b \:0432 \:043e\:043f\:0440\:0435\:0434\:0435\:043b\:0435\:043d\:043d\:044b\:0439 \:0433\:043e\:0434*)
(*getMeetingDemandVisualisationByQuarter -  \:0421\:043e\:0441\:0442\:0430\:0432\:043b\:0435\:043d\:0438\:0435 \:043a\:0440\:0443\:0433\:043e\:0432\:043e\:0439 \:0434\:0438\:0430\:0433\:0440\:0430\:043c\:043c\:044b, \:043e\:0442\:0440\:0430\:0436\:0430\:044e\:0449\:0435\:0439 \:0437\:0430 \:0441\:0447\:0435\:0442 \:0447\:0435\:0433\:043e \:043f\:0440\:043e\:0438\:0441\:0445\:043e\:0434\:0438\:0442 \:0443\:0434\:043e\:0432\:043b\:0435\:0442\:0432\:043e\:0440\:0435\:043d\:0438\:044f \:043f\:043e\:0442\:0440\:0435\:0431\:043d\:043e\:0441\:0442\:0438 \:0432 \:0433\:0430\:0437\:0435 \:0434\:043b\:044f \:043e\:043f\:0440\:0435\:0434\:0435\:043b\:0435\:043d\:043d\:043e\:0439 \:0441\:0442\:0440\:0430\:043d\:044b \:0432 \:043e\:043f\:0440\:0435\:0434\:0435\:043b\:0435\:043d\:043d\:044b\:0439 \:043a\:0432\:0430\:0440\:0442\:0430\:043b (1-4) \:0432 \:043e\:043f\:0440\:0435\:0434\:0435\:043b\:0435\:043d\:043d\:044b\:0439 \:0433\:043e\:0434*)


getMeetingDemandVisualisationByMonth[country_String, date_String] :=
    getMeetingDemandVisualisation[getBalanceDataByMonth[country, date
        ], date]

getMeetingDemandVisualisationByYear[country_String, date_String] /; 2018 <=
     ToExpression @ date <= 2021 :=
    getMeetingDemandVisualisation[aggregate @ getBalanceDataByYear[country,
         date], date]

getMeetingDemandVisualisationByQuarter[country_String, date_String, quarter_Integer
    ] /; 1 <= quarter <= 4 :=
    getMeetingDemandVisualisation[getBalanceDataByQuarter[country, date,
         quarter], date]


(* ::Section:: *)
(*getMeetingDemandVisualisation - \:0421\:043e\:0441\:0442\:0430\:0432\:043b\:0435\:043d\:0438\:0435 \:043a\:0440\:0443\:0433\:043e\:0432\:043e\:0439 \:0434\:0438\:0430\:0433\:0440\:0430\:043c\:043c\:044b, \:043e\:0442\:0440\:0430\:0436\:0430\:044e\:0449\:0435\:0439 \:0437\:0430 \:0441\:0447\:0435\:0442 \:0447\:0435\:0433\:043e \:043f\:0440\:043e\:0438\:0441\:0445\:043e\:0434\:0438\:0442 \:0443\:0434\:043e\:0432\:043b\:0435\:0442\:0432\:043e\:0440\:0435\:043d\:0438\:044f \:043f\:043e\:0442\:0440\:0435\:0431\:043d\:043e\:0441\:0442\:0438 \:0432 \:0433\:0430\:0437\:0435 \:0434\:043b\:044f \:043e\:043f\:0440\:0435\:0434\:0435\:043b\:0435\:043d\:043d\:043e\:0439 \:0441\:0442\:0440\:0430\:043d\:044bcomparativeVisualisation - \:0421\:043e\:0441\:0442\:0430\:0432\:043b\:0435\:043d\:0438\:044f \:0433\:0438\:0441\:0442\:043e\:0433\:0440\:0430\:043c\:043c\:044b, \:0441\:043e\:043f\:043e\:0441\:0442\:0430\:0432\:043b\:044f\:044e\:0449\:0435\:0439 \:0434\:0430\:043d\:043d\:044b\:0435 \:0442\:0430\:043a\:0438\:0435 \:043a\:0430\:043a, \:0438\:043c\:043f\:043e\:0440\:0442-\:044d\:043a\:0441\:043f\:043e\:0440\:0442, \:0434\:043e\:0431\:044b\:0447\:0430-\:043f\:043e\:0442\:0440\:0435\:0431\:043b\:0435\:043d\:0438\:0435, \:0437\:0430\:043a\:0430\:0447\:043a\:0430 \:0432 \:041f\:0425\:0413-\:0438\:0437\:044a\:044f\:0442\:0438\:0435 \:0438\:0437 \:041f\:0425\:0413 \:0434\:043b\:044f \:043e\:043f\:0440\:0435\:0434\:0435\:043b\:0435\:043d\:043d\:043e\:0439 \:0441\:0442\:0440\:0430\:043d\:044b*)


getMeetingDemandVisualisation[data_, date_] :=
  Module[{
    legend = {"\:0420\:0430\:0437\:043d\:043e\:0441\:0442\:044c \:0438\:043c\:043f\:043e\:0440\:0442\:0430 \:0438 \:044d\:043a\:0441\:043f\:043e\:0440\:0442\:0430", "\:0414\:043e\:0431\:044b\:0447\:0430", "\:0418\:0437\:044a\:044f\:0442\:0438\:0435 \:0438\:0437 \:0421\:041f\:0413",
       "\:0418\:0437\:044a\:044f\:0442\:0438\:0435 \:0438\:0437 \:041f\:0425\:0413"}
    ,
    totalDemand =
      Round[
        If[data["import"] - data["export"] >= 0,
          data["consumption"] + data["ugsIngection"] + data["statmistake"
            ]
          ,
          data["consumption"] + data["ugsIngection"] + data["export"]
             - data["import"] + data["statmistake"]
        ]
        ,
        2
      ]
  },
    If[data["import"] - data["export"] >= 0,
      PieChart[{data["import"] - data["export"], data["production"], 
        data["lngSendOut"], data["ugsWithdrawal"]}, ChartLegends -> SwatchLegend[
        legend, LegendMarkerSize -> 24, LabelStyle -> Directive[FontSize -> 32
        ]], SectorOrigin -> {Automatic, 1}, PlotLabel -> "\:0423\:0434\:043e\:0432\:043b\:0435\:0442\:0432\:043e\:0440\:0435\:043d\:0438\:0435 \:0441\:043f\:0440\:043e\:0441\:0430\n \:043d\:0430 \:0433\:0430\:0437 \:0437\:0430 "
         <> date <> "\n" <> ToString[totalDemand] <> " GWh", LabelingFunction
         -> (Callout[Row[{NumberForm[(# / totalDemand) * 100, {2, 2}], "%"}],
         Automatic]&), LabelStyle -> Directive[FontSize -> 18], ImageSize -> 
        {1000, 600}]
      ,
      PieChart[{data["production"], data["IngSendOut"], data["ugsWithdrawal"
        ]}, ChartLegends -> SwatchLegend[legend[[2 ;; ]], LegendMarkerSize ->
         24, LabelStyle -> Directive[FontSize -> 32]], SectorOrigin -> {Automatic,
         1}, PlotLabel -> "\:0423\:0434\:043e\:0432\:043b\:0435\:0442\:0432\:043e\:0440\:0435\:043d\:0438\:0435 \:0441\:043f\:0440\:043e\:0441\:0430\n \:043d\:0430 \:0433\:0430\:0437 \:0437\:0430 " <> date <> "\n"
         <> ToString[totalDemand] <> " GWh", LabelingFunction -> (Callout[Row[
        {NumberForm[(# / totalDemand) * 100, {2, 2}], "%"}], Automatic]&), LabelStyle
         -> Directive[FontSize -> 18], ImageSize -> {1000, 600}]
    ]
  ]

comparativeVisualisation[data_, date_] :=
  Module[{legend = {{"\:0418\:043c\:043f\:043e\:0440\:0442", "\:042d\:043a\:0441\:043f\:043e\:0440\:0442"}, {"\:0414\:043e\:0431\:044b\:0447\:0430", "\:041f\:043e\:0442\:0440\:0435\:0431\:043b\:0435\:043d\:0438\:0435"},
     {"\:0418\:0437\:044a\:044f\:0442\:0438\:0435 \:0438\:0437 \:041f\:0425\:0413", "\:0417\:0430\:043a\:0430\:0447\:043a\:0430 \:041f\:0425\:0413"}}, labels = {{"\:0421\:0440\:0430\:0432\:043d\:0435\:043d\:0438\:0435\n\:0438\:043c\:043f\:043e\:0440\:0442\:0430 \:0438 \:044d\:043a\:0441\:043f\:043e\:0440\:0442\:0430 \:0437\:0430\n"
     <> date}, {"\:0421\:0440\:0430\:0432\:043d\:0435\:043d\:0438\:0435\n\:0434\:043e\:0431\:044b\:0447\:0438 \:0438 \:043f\:043e\:0442\:0440\:0435\:0431\:043b\:0435\:043d\:0438\:044f \:0437\:0430\n" <> date}, {"\:0421\:0440\:0430\:0432\:043d\:0435\:043d\:0438\:0435\n\:0438\:0437\:044a\:044f\:0442\:0438\:044f \:0438 \:0437\:0430\:043a\:0430\:0447\:0438 \:0432 \:041f\:0425\:0413 \:0437\:0430\n"
     <> date}}},
    GraphicsColumn[MapIndexed[BarChart[#1, ChartLegends -> SwatchLegend[
      legend[[#2]], LegendMarkerSize -> 20, LabelStyle -> Directive[FontSize
       -> 32]], ChartStyle -> "DarkRainbow", PlotLabel -> First @ labels[[#2,
       1]], AxesLabel -> "GWh", LabelStyle -> Directive[FontSize -> 18]]&, 
      {{data["import"], data["export"]}, {data["production"], data["consumption"
      ]}, {data["ugsWithdrawal"], data["ugsIngection"]}}], ImageSize -> {1500,
       1500}]
  ]


(* ::Section:: *)
(*getDataByMonth - \:041f\:043e\:043b\:0443\:0447\:0435\:043d\:0438\:0435 \:0431\:0430\:043b\:0430\:043d\:0441\:043e\:0432\:044b\:0445 \:0434\:0430\:043d\:043d\:044b\:0445 \:043f\:043e \:0441\:0442\:0440\:0430\:043d\:0435 \:0437\:0430 \:043e\:043f\:0440\:0435\:0434\:0435\:043b\:0435\:043d\:043d\:044b\:0439 \:043c\:0435\:0441\:044f\:0446*)
(*getDataByYear - \:041f\:043e\:043b\:0443\:0447\:0435\:043d\:0438\:0435 \:0431\:0430\:043b\:0430\:043d\:0441\:043e\:0432\:044b\:0445 \:0434\:0430\:043d\:043d\:044b\:0445 \:043f\:043e \:0441\:0442\:0440\:0430\:043d\:0435 \:0437\:0430 \:043e\:043f\:0440\:0435\:0434\:0435\:043b\:0435\:043d\:043d\:044b\:0439 \:0433\:043e\:0434*)
(*geyDataByQuarter - \:041f\:043e\:043b\:0443\:0447\:0435\:043d\:0438\:0435 \:0431\:0430\:043b\:0430\:043d\:0441\:043e\:0432\:044b\:0445 \:0434\:0430\:043d\:043d\:044b\:0445 \:043f\:043e \:0441\:0442\:0440\:0430\:043d\:0435 \:0437\:0430 \:043e\:043f\:0440\:0435\:0434\:0435\:043b\:0435\:043d\:043d\:044b\:0439 \:043a\:0432\:0430\:0440\:0442\:0430\:043b (1-4) \:0437\:0430 \:043e\:043f\:0440\:0435\:0434\:0435\:043b\:0435\:043d\:043d\:044b\:0439 \:0433\:043e\:0434 *)
(*aggregate - \:0421\:0443\:043c\:043c\:0438\:0440\:043e\:0432\:0430\:043d\:0438\:0435 \:0432\:0441\:0435\:0445 \:0434\:0430\:043d\:043d\:044b\:0445 \:0437\:0430 \:0433\:043e\:0434 \:0438\:043b\:0438 \:043a\:0432\:0430\:0440\:0442\:0430\:043b*)


getBalanceDataByMonth[country_, date_] :=
    getBalanceData[][country, date]

getBalanceDataByYear[country_, year_] :=
    getBalanceData[][country][[Normal @ Select[Keys @ (getBalanceData[
        ][country]), StringContainsQ[#, year]&]]]

getBalanceDataByQuarter[country_, year_, quarter_] :=
    (Reverse @ getBalanceDataByYear[country, year])[[(3 * quarter - 2
        ) ;; (3 * quarter)]] // aggregate

aggregate[data_Association] :=
    Dataset @ Map[Association @@ #&, Normal @ Values @ data] // Total


(* ::Section:: *)
(*\:041a\:043e\:043d\:0435\:0447\:043d\:044b\:0435 \:0444\:0443\:043d\:043a\:0446\:0438\:0438 \:0434\:043b\:044f \:043f\:0440\:043e\:0440\:0438\:0441\:043e\:0432\:043a\:0438 \:0433\:0440\:0430\:0444\:0438\:043a\:043e\:0432*)


visualisation::invalidArgs="\:0412\:044b \:0432\:0432\:0435\:043b\:0438 \:043d\:0435\:0432\:0435\:0440\:043d\:044b\:0435 \:0430\:0440\:0433\:0443\:043c\:0435\:043d\:0442\:044b";
visualisation::invalidDate = "\:0413\:043e\:0434 \:0434\:043e\:043b\:0436\:0435\:043d \:0431\:044b\:0442\:044c \:0432 \:043f\:0440\:0435\:0434\:0435\:043b\:0435 \:043e\:0442 2018 \:0434\:043e 2021";
visualisation::invalidKey = "\:0422\:0430\:043a\:043e\:0433\:043e \:043a\:043e\:0434\:0430 \:0441\:0442\:0440\:0430\:043d\:044b \:043d\:0435 \:0441\:0443\:0449\:0435\:0441\:0442\:0432\:0443\:0435\:0442";


getMeetingDemandByYear[country_String, year_String] /; 2018 <= ToExpression @
     year <= 2021 \[And] balanceKeysMemberQ[country] :=
    TabView[{"\:0413\:043e\:0434" -> getMeetingDemandVisualisationByYear[country, year
        ], "\:041a\:0432\:0430\:0440\:0442\:0430\:043b" -> TabView @ Map[ToString @ # <> " \:043a\:0432\:0430\:0440\:0442\:0430\:043b" -> getMeetingDemandVisualisationByQuarter[
        country, year, #]&, Range @ 4], "\:041c\:0435\:0441\:044f\:0446\:044b" -> TabView @ Map[# -> getMeetingDemandVisualisationByMonth[
        country, #]&, Normal @ Select[Keys @ (getBalanceData[][country]), StringContainsQ[
        #, year]&]]}]

getMeetingDemandByYear[country_String, year_String] /; 2018 <= ToExpression @
     year <= 2021 \[And] \[Not]balanceKeysMemberQ[country] :=
    Message[visualisation::invalidKey]

getMeetingDemandByYear[country_String, year_String] /; \[Not](2018 <= ToExpression
     @ year <= 2021) \[And] balanceKeysMemberQ[country] :=
    Message[visualisation::invalidDate]

getMeetingDemandByYear[args___] :=
    Message[visualisation::invalidArgs]

getComparativeByYear[country_String, year_String] /; 2018 <= (ToExpression @
     year) <= 2021 \[And] balanceKeysMemberQ[country] :=
    TabView[{"\:0413\:043e\:0434" -> comparativeVisualisationByYear[country, year], 
        "\:041a\:0432\:0430\:0440\:0442\:0430\:043b" -> TabView @ Map[ToString @ # <> " \:043a\:0432\:0430\:0440\:0442\:0430\:043b" -> comparativeVisualisationByQuarter[
        country, year, #]&, Range @ 4], "\:041c\:0435\:0441\:044f\:0446\:044b" -> TabView @ Map[# -> comparativeVisualisationByMonth[
        country, #]&, Normal @ Select[Keys @ (getBalanceData[][country]), StringContainsQ[
        #, year]&]]}]

getComparativeByYear[country_String, year_String] /; 2018 <= ToExpression @
     year <= 2021 \[And] \[Not]balanceKeysMemberQ[country] :=
    Message[visualisation::invalidKey]

getComparativeByYear[country_String, year_String] /; \[Not](2018 <= ToExpression
     @ year <= 2021) \[And] balanceKeysMemberQ[country] :=
    Message[visualisation::invalidDate]

getComparativeByYear[args___] :=
    Message[visualisation::invalidArgs]


(* ::Section:: *)
(*\:041f\:0440\:0435\:043e\:0431\:0440\:0430\:0437\:043e\:0432\:0430\:043d\:0438\:044f \:043a\:043e\:0434\:043e\:0432 \:0441\:0442\:0440\:0430\:043d \:0432 \:043d\:043e\:0440\:043c\:0430\:043b\:044c\:043d\:044b\:0435 \:0432\:044b\:0440\:0430\:0436\:0435\:043d\:0438\:044f \:0441 \:0433\:043e\:043b\:043e\:0432\:043e\:0439 Entity*)


entityCountries = {Entity["Country","Portugal"],Entity["Country","Netherlands"],Entity["Country","Italy"],Entity["Country","Belgium"],Entity["Country","Spain"],Entity["Country","Turkey"],Entity["Country","UnitedKingdom"],Entity["Country","Croatia"],Entity["Country","Germany"],Entity["Country","Hungary"],Entity["Country","Romania"],Entity["Country","Sweden"],Entity["Country","Slovenia"],Entity["Country","Slovakia"],Entity["Country","Bulgaria"],Entity["Country","France"],Entity["Country","Luxembourg"],Entity["Country","Norway"],Entity["Country","Poland"],Entity["Country","Serbia"],Entity["Country","Lithuania"],Entity["Country","Austria"],Entity["Country","CzechRepublic"],Entity["Country","Denmark"],Entity["Country","Finland"],Entity["Country","Latvia"],Entity["Country","Malta"],Entity["Country","Estonia"],Entity["Country","Georgia"],Entity["Country","Moldova"],Entity["Country","Ukraine"],Entity["Country","Macedonia"],Entity["Country","Ireland"],Entity["Country","Albania"],Entity["Country","Cyprus"],Entity["Country","Iceland"]};


(* ::Section:: *)
(*getParamVisualisation - \:0414\:0430\:0435\:0442 \:0432\:0438\:0437\:0443\:0430\:043b\:0438\:0437\:0430\:0446\:0438\:044e \:043d\:0430 \:043a\:0430\:0440\:0442\:0435 \:0415\:0432\:0440\:043e\:043f\:044b \:0434\:043b\:044f \:043a\:043e\:043d\:043a\:0440\:0435\:0442\:043d\:043e\:0433\:043e \:043f\:0430\:0440\:0430\:043c\:0435\:0442\:0440\:0430*)
(**)
(*getParamVisualisationByMonth - \:0412\:0438\:0437\:0443\:0430\:043b\:0438\:0437\:0430\:0446\:0438\:044f \:043f\:0430\:0440\:0430\:043c\:0435\:0442\:0440\:0430 \:0437\:0430 \:043c\:0435\:0441\:044f\:0446*)
(**)
(*getParamVisualisationByQuarter - \:0412\:0438\:0437\:0443\:0430\:043b\:0438\:0437\:0430\:0446\:0438\:044f \:043f\:0430\:0440\:0430\:043c\:0435\:0442\:0440\:0430 \:0437\:0430 \:043a\:0432\:0430\:0440\:0442\:0430\:043b*)
(**)
(*getParamVisualisationByYear - \:0412\:0438\:0437\:0443\:0430\:043b\:0438\:0437\:0430\:0446\:0438\:044f \:043f\:0430\:0440\:0430\:043c\:0435\:0442\:0440\:0430 \:0437\:0430 \:0433\:043e\:0434*)


getParamVisualisation[data_, param_] :=
    GeoRegionValuePlot[MapIndexed[First @ entityCountries[[#2]] -> #1&,
         data], GeoLabels -> Automatic, PlotLabel -> Style[Framed @ param, 28,
         Bold], TargetUnits -> "GwH", ImageSize -> {1366, 768}, LabelStyle ->
         {FontSize -> 18}, GeoRange -> {GeoPosition[Entity["Country", "Iceland"
        ]], GeoPosition[Entity["Country", "Georgia"]]}]

getParamVisualisationByMonth[date_, param_] :=
    getParamVisualisation[Map[getBalanceDataByMonth[#, date][param]&,
         Normal @ Keys @ getBalanceData[]] /. Missing[x__] -> 0, param]

ClearAll[getParamVisualisationByQuarter]

getParamVisualisationByQuarter[year_, quarter_, param_] /; 1 <= quarter <=
     4 :=
    getParamVisualisation[Map[getBalanceDataByQuarter[#1, year, quarter
        ][param]&, Normal @ Keys @ getBalanceData[]] /. Missing[x__] -> 0, param
        ]

ClearAll[getParamVisualisationByYear]

getParamVisualisationByYear[year_, param_] /; 2018 <= ToExpression @ year <=
     2021 :=
    getParamVisualisation[Map[(aggregate @ getBalanceDataByYear[#1, year
        ])[param]&, Normal @ Keys @ getBalanceData[]] /. Missing[x__] -> 0, param
        ]


(* ::Section:: *)
(*\:041a\:043e\:043d\:0435\:0447\:043d\:0430\:044f \:0444\:0443\:043d\:043a\:0446\:0438\:044f \:0434\:043b\:044f \:0440\:0438\:0441\:043e\:0432\:0430\:043d\:0438\:044f \:0433\:0440\:0430\:0444\:0438\:043a\:0430*)


getGeoParamVisualisationByYear::invalidDate = "\:0413\:043e\:0434 \:0434\:043e\:043b\:0436\:0435\:043d \:0431\:044b\:0442\:044c \:0432 \:043f\:0440\:0435\:0434\:0435\:043b\:0435 \:043e\:0442 2018 \:0434\:043e 2021"

getGeoParamVisualisationByYear::invalidArg = "\:0412\:044b \:0432\:0432\:0435\:043b\:0438 \:043d\:0435\:043f\:043e\:0434\:0445\:043e\:0434\:044f\:0449\:0438\:0435 \:043f\:0430\:0440\:0430\:043c\:0435\:0442\:0440\:044b"


getTabView[param_, year_] :=
    TabView @ {"\:0413\:043e\:0434" -> getParamVisualisationByYear[year, param], "\:041a\:0432\:0430\:0440\:0442\:0430\:043b"
         -> TabView @ Map[ToString @ # <> " \:043a\:0432\:0430\:0440\:0442\:0430\:043b" -> getParamVisualisationByQuarter[
        year, #, param]&, Range @ 4]}

getGeoParamVisualisationByYear[year_String] /; 2018 <= ToExpression @
     year <= 2021 :=
    TabView[Map[# -> getTabView[#, year]&, {"import", "export", "production",
         "consumption", "ugsWithdrawal", "ugsIngection"}]]

getGeoParamVisualisationByYear[year_String] :=
    Message[getGeoParamVisualisationByYear::invalidDate]

getGeoParamVisualisationByYear[args___] :=
    Message[getGeoParamVisualisationByYear::invalidArgs]


(* ::Section:: *)
(*replaceMissing - \:0437\:0430\:043c\:0435\:043d\:044f\:0435\:0442 \:0434\:0430\:043d\:043d\:044b\:0435, \:043a\:043e\:0442\:043e\:0440\:044b\:0435 \:0441\:043e\:0434\:0435\:0440\:0436\:0430\:0442 Missing (replaceMissing[331.123 + Missing[...]] ----> 331.123)*)
(*convertData - \:0432\:043e\:0437\:0432\:0440\:0430\:0449\:0430\:0435\:0442 Dataset \:0441\:043e \:0441\:0442\:043e\:043b\:0431\:0446\:0430\:043c\:0438: {Country, Date, import, ugsWithdrawal, production ,lngSendOut, export, ugsInjection, consumption, statmistake }*)
(*exportToXlsx - \:044d\:043a\:0441\:043f\:043e\:0440\:0442\:0438\:0440\:0443\:0435\:0442 \:0434\:0430\:043d\:043d\:044b\:0435 \:043f\:043e \:043e\:043f\:0440\:0435\:0434\:0435\:043b\:0435\:043d\:043d\:043e\:0439 \:0441\:0442\:0440\:0430\:043d\:0435 \:0432 xlsx*)
(*exportAllToXlsx - \:044d\:043a\:0441\:043f\:043e\:0440\:0442\:0438\:0440\:0443\:0435\:0442 \:0434\:0430\:043d\:043d\:044b\:0435 \:043e \:0432\:0441\:0435\:0445 \:0441\:0442\:0440\:0430\:043d\:0430\:0445 \:0432 xlsx*)


replaceMissing[number_] :=
    If[NumberQ @ number,
        number
        ,
        First @ number
    ]


convertData[country_] :=
    Module[{dates = Normal @ Keys @ getBalanceData[][country], data = Dataset 
        @ Map[Association @@ #&, Normal @ Values @ getBalanceData[][country]]},
        (Dataset @ MapIndexed[Join @@ ({<|"Country" -> country, "Date" -> #1|>} ~ Join ~ data
            [[#2]])&, dates])[All, {"statmistake" -> replaceMissing, "percentmistake"
             -> ToString}]
    ]


exportToXlsx[country_String] /; balanceKeysMemberQ[country] :=
    Export[country <> "_balance.xlsx", country->convertData[country]]

exportToXlsx[country_String] /; balanceKeysMemberQ[country] :=
    Message["\:0422\:0430\:043a\:043e\:0433\:043e \:043a\:043e\:0434\:0430 \:0441\:0442\:0440\:0430\:043d\:044b \:043d\:0435\:0442 \:0441\:0440\:0435\:0434\:0438 \:043a\:043e\:0434\:043e\:0432 \:0441\:0442\:0440\:0430\:043d \:0431\:0430\:043b\:0430\:043d\:0441\:043e\:0432\:043e\:0439 \:0442\:0430\:0431\:043b\:0438\:0446\:044b"
        ]

exportToXlsx[args___] :=
    Message[getData::invalidArgs]


exportAllToXlsx[] :=
    Export["all_balance.xlsx", Join @@ Map[convertData, Keys @ getBalanceData[
        ]]]

exportAllToXlsx[args__] :=
    Message[getData::invalidArgs]


End[]


EndPackage[]
