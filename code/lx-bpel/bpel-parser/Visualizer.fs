module Visualizer

open FSharp.Charting

let show samplesSeq =
    let bucketCount = 30

    let bucketize rangeMin rangeMax : seq<float> -> seq<float*int> =
        let range = rangeMax - rangeMin
        let bucketWidth = range / (float bucketCount)
        Seq.countBy <| fun v ->
            ((v - rangeMin) / bucketWidth |> truncate) * bucketWidth  + (rangeMin + bucketWidth/2.0)

    let reliability =
        samplesSeq
        |> Seq.map (fun (environment,outcome,cost) -> outcome)
        |> Seq.averageBy (function lx_bpel.Success -> 1.0 | _ -> 0.0 )

    let pie =
        samplesSeq
        |> Seq.map (fun (environment,outcome,cost) -> outcome)
        |> Seq.countBy id
        |> Seq.map (fun (outcome,number) -> sprintf "%+A" outcome,number)
        |> fun x ->  Chart.Pie (x,Title=(sprintf "Reliability: %.1f%%" (reliability*100.0) ))

    let priceAvg =
        samplesSeq
        |> Seq.averageBy (fun (environment,outcome,(price,time)) -> price)
    let histogramPriceAvg =
        let samplesSeq =
            samplesSeq
            |> Seq.map (fun (environment,outcome,(price,time)) -> price)
        let rMin = Seq.min samplesSeq
        let rMax = Seq.max samplesSeq
        samplesSeq
        |> bucketize rMin rMax
        |> Seq.maxBy snd
        |> fun (_,my) ->[ priceAvg,0.0;priceAvg,float my * 1.1]
        |> fun x -> Chart.Line (x,null,null,["";sprintf "Average price ($): %.2f" priceAvg],System.Drawing.Color.Red)
        //|> Chart.WithXAxis (null,null,null,null,null,null,null,)
    let histogramPrice =
        let samplesSeq =
            samplesSeq
            |> Seq.map (fun (environment,outcome,(price,time)) -> price)
        let rMin = Seq.min samplesSeq
        let rMax = Seq.max samplesSeq
        samplesSeq
        |> bucketize rMin rMax
        |> Seq.sortBy fst
        |>fun x -> Chart.Column (x,Color=System.Drawing.Color.Blue)
        |> fun x -> Chart.Combine [x;histogramPriceAvg]
        |> Chart.WithXAxis (true,"Cost ($)")
    let timeAvg =
        samplesSeq
        |> Seq.averageBy (fun (environment,outcome,(price,time)) -> time)
    let histogramTimeAvg =
        let samplesSeq =
            samplesSeq
            |> Seq.map (fun (environment,outcome,(price,time)) -> time)
        let rMin = Seq.min samplesSeq
        let rMax = Seq.max samplesSeq
        samplesSeq
        |> bucketize rMin rMax
        |> Seq.maxBy snd
        |> fun (_,my) ->[ priceAvg,0.0;priceAvg,float my * 1.1]
        |> fun x -> Chart.Line (x,null,null,["";sprintf "Average time (sec): %.0f" priceAvg],System.Drawing.Color.Red)
        //|> Chart.WithXAxis (null,null,null,null,null,null,null,)

    let histogramTime =
        let samplesSeq =
            samplesSeq
            |> Seq.map (fun (environment,outcome,(price,time)) -> time)
        let rMin = Seq.min samplesSeq
        let rMax = Seq.max samplesSeq
        samplesSeq
        |> bucketize rMin rMax
        |> Seq.sortBy fst
        |>fun x -> Chart.Column (x,Color=System.Drawing.Color.Blue)
        |> fun x -> Chart.Combine [x;histogramTimeAvg]
        |> Chart.WithXAxis (true,"Time (sec)")


    let allGraphs = Chart.Columns ([pie;Chart.Rows [histogramPrice;histogramTime]])
    //let control = new ChartTypes.ChartControl(pie)
    let form = allGraphs.ShowChart() // new System.Windows.Forms.Form()
    //form.Controls.Add(control)
    form.WindowState <- System.Windows.Forms.FormWindowState.Maximized
    System.Windows.Forms.Application.Run(form)