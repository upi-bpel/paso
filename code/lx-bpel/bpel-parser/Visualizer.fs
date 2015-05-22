module Visualizer

open FSharp.Charting

let show (simulatedSamples:int) (distribution:lx_bpel.TMonad<Map<string,bool>*lx_bpel.Outcome*lx_bpel.Cost>) =
    (*
    let bucketCount = 30

    let bucketize rangeMin rangeMax  (input: seq<float>): seq<float*int> =
        let range = rangeMax - rangeMin
        let bucketWidth = range / (float bucketCount)
        let cseq = Seq.countBy <| fun v ->
                ((v - rangeMin) / bucketWidth |> int)
        cseq input
        |> Seq.map (fun (k,v) -> (float k) * bucketWidth  + (rangeMin + bucketWidth/2.0),v)


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
            |> Seq.cache
        let rMin = Seq.min samplesSeq
        let rMax = Seq.max samplesSeq
        let range = rMax - rMin
        let rMin = ((rMin / range) * 100.0 |> truncate) * range / 100.0
        let rMax = ((rMax / range) * 100.0 + 0.9999 |> truncate) * range / 100.0
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
            |> Seq.cache
        let rMin = Seq.min samplesSeq
        let rMax = Seq.max samplesSeq
        samplesSeq
        |> bucketize rMin rMax
        |> Seq.sortBy fst
        |> fun x -> Chart.Column (x,Color=System.Drawing.Color.Blue)
        |> fun x -> Chart.Combine [x;histogramPriceAvg]
        |> Chart.WithXAxis (true,"Cost ($)")
    let timeAvg =
        samplesSeq
        |> Seq.filter (fun (e,o,c) -> o <> lx_bpel.Stuck)
        |> Seq.averageBy (fun (environment,outcome,(price,time)) -> time)
    let histogramTimeAvg =
        let samplesSeq =
            samplesSeq
            |> Seq.filter (fun (e,o,c) -> o <> lx_bpel.Stuck)
            |> Seq.map (fun (environment,outcome,(price,time)) -> time)
            |> Seq.cache
        let rMin = Seq.min samplesSeq
        let rMax = Seq.max samplesSeq
        let range = rMax - rMin
        let rMin = ((rMin / range) * 100.0 |> truncate) * range / 100.0
        let rMax = ((rMax / range) * 100.0 + 0.9999 |> truncate) * range / 100.0
        samplesSeq
        |> bucketize rMin rMax
        |> Seq.maxBy snd
        |> fun (_,my) ->[ timeAvg,0.0;timeAvg,float my * 1.1]
        |> fun x -> Chart.Line (x,null,null,["";sprintf "Average time (sec): %.1f" timeAvg],System.Drawing.Color.Red)
        //|> Chart.WithXAxis (null,null,null,null,null,null,null,)

    let histogramTime =
        let samplesSeq =
            samplesSeq
            |> Seq.filter (fun (e,o,c) -> o <> lx_bpel.Stuck)
            |> Seq.map (fun (environment,outcome,(price,time)) -> time)
            |> Seq.cache
        let rMin = Seq.min samplesSeq
        let rMax = Seq.max samplesSeq
        samplesSeq
        |> bucketize rMin rMax
        |> Seq.sortBy fst
        |>fun x -> Chart.Column (x,Color=System.Drawing.Color.Blue)
        |> fun x -> Chart.Combine [x;histogramTimeAvg]
        |> Chart.WithXAxis (true,"Time (sec)")
    *)
    let pie =
        lx_bpel.Eval.dist {
            let! (env,outcome,cost) = distribution
            return sprintf "%+A" outcome
        }
        |> lx_bpel.Eval.listlr
        |> Chart.Pie
        (*let smap (environment,outcome,cost) =
            Map.empty.Add(outcome,1.0)
        let rec smul scale m =
            Map.map (fun outcome weight -> weight*scale) m
        let ssum m1 m2 =
            let fder acc key value =
                match Map.tryFind key acc with
                | None ->  Map.add key value acc
                | Some v -> Map.add key (v  + value) acc
            Map.fold fder m1 m2
        distribution.intfun smap smul ssum
        |> Map.toList
        |> Chart.Pie*)

    let rangr (d:lx_bpel.TMonad<'T>) =
        d.intfun (fun v -> (v,v)) (fun x -> id) (fun (a,b) (c,d) -> (min a c),(max b d))
    
    let buckets vald = 
        let bucketCount = 30
        let rangeMin,rangeMax = rangr vald
        let range = rangeMax - rangeMin
        let bucketWidth = range / (float bucketCount)
        lx_bpel.Eval.dist {
            let! v = vald
            return ((v - rangeMin) / bucketWidth |> truncate) * bucketWidth  + (rangeMin + bucketWidth/2.0)
        }
        |> lx_bpel.Eval.listlr
        |> List.map (fun (a,b)-> a,(float simulatedSamples) * b)
    let histogramPrice =
        let priced =
            lx_bpel.Eval.dist {
                let! (env,outcome,(price,time)) = distribution
                return price
            }
        let b = buckets priced
        let avg = priced.intfun id (*) (+)
        let bardata = 
            let _,maxHeight = List.maxBy snd b
            [avg,0.0;avg,maxHeight * 1.1]
        let line = Chart.Line (bardata,null,null,["";sprintf "Average price ($): %.2f" avg],System.Drawing.Color.Red)
        let histogram = Chart.Column (b,Color=System.Drawing.Color.Blue)
        Chart.Combine [line;histogram]
    let histogramTime =
        let timed =
            lx_bpel.Eval.dist {
                let! (env,outcome,(price,time)) = distribution
                return time
            }
        let b = buckets timed
        let avg = timed.intfun id (*) (+)
        let bardata = 
            let _,maxHeight = List.maxBy snd b
            [avg,0.0;avg,maxHeight * 1.1]
        let line = Chart.Line (bardata,null,null,["";sprintf "Average price ($): %.2f" avg],System.Drawing.Color.Red)
        let histogram = Chart.Column (b,Color=System.Drawing.Color.Blue)
        Chart.Combine [line;histogram]
    let allGraphs = Chart.Columns ([pie;Chart.Rows [histogramPrice;histogramTime]])
    //let control = new ChartTypes.ChartControl(pie)
    let form = allGraphs.ShowChart() // new System.Windows.Forms.Form()
    //let form = histogramTime.ShowChart()
    //form.Controls.Add(control)
    //form.WindowState <- System.Windows.Forms.FormWindowState.Maximized
    form.TopMost <- false
    System.Windows.Forms.Application.Run(form)