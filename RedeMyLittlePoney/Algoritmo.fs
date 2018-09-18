namespace RedeMyLittlePoney

open System
open System.Diagnostics
open System.Collections.Generic

open FSharp.Data
open MathNet.Numerics
open MathNet.Numerics.Random
open MathNet.Numerics.LinearAlgebra
open MathNet.Numerics.Statistics
open FSharp.Collections.ParallelSeq

module Algoritmo =

    //Tipos
    type Par = { X: float Vector; Y: float Vector }
    type Modelo = { I: float Vector list; J: float Vector list }
    type Entrada = { Dados: Par list; Classes: Vector<float> list; NumeroNeuronios: float; TaxaAprendizado: int }
    
    type Realizacao = { TaxaAcerto:float; Confusao: float Matrix; W: Modelo }
    type RealizacaoRegressao = { RMSE :float; W: Modelo }

    type ResultadoClassificacao = { Acuracia: float; DesvioPadrao: float; Melhor: Realizacao; }
    type ResultadoRegressao = { RMSE: float; DesvioPadrao: float; Melhor: RealizacaoRegressao; }

    type ResultadoParametros = { NumeroNeuronios: int; TaxaAprendizado: float; Precisao: float }
    
    //Funções
    let sigmoide x = 
        1.0 /  (1.0 + Math.Pow(Math.E, -x))
    
    let sigmoide' x = 
        //Differentiate.derivative 1 x sigmoide
        (sigmoide x) * (1.0 - (sigmoide x))
    
    let linear x =
        x

    let linear' _ =
        1.0

    let funcaoRegessao x =
        3.0 * Math.Sin(x) + 1.0
    
    let ponderada w x =
        x .* w |> Vector.sum

    let saida w x ativacao =
        (ativacao: float -> float) |> ignore
        ponderada w x |> ativacao

    let saidaCamadaI c x =
        let map w = saida w x sigmoide
        let saida = c |> List.map map
        -1.0 :: saida |> vector
        
    let saidaCamadaJ c x ativacao =
        let map w = saida w x ativacao
        c |> Seq.map map |> vector
    
    let resultado m x ativacao =
        let xj = saidaCamadaI m.I x
        let y = saidaCamadaJ m.J xj ativacao
        y |> Seq.map (fun n -> Math.Round n) |> vector
    
    let resultadoSigmoide m x =
        resultado m x sigmoide

    let resultadoLinear m x =
        let xj = saidaCamadaI m.I x
        let y = saidaCamadaJ m.J xj linear
        y
    
    let normaliza x min max =
        (x - min) / (max - min)

    let sw = new Stopwatch();
    
    //Próximo modelo para o vetor "treinamento"
    let pesos dados numSaidas ativacao ativacao' numNeuronios taxaAjuste  =
        (dados: Par list) |> ignore

        //Épocas
        let maxN = 200
        
        //Próximo modelo (função w(n+1))
        let rec proximo t m = 
            match t with
                | [] -> m
                | par :: tail -> 
                    let xj = saidaCamadaI m.I par.X
                    let y = saidaCamadaJ m.J xj ativacao
                    let erro = par.Y - y

                    let wjMap j wj = 
                        let e = erro.[j]
                        let f'u = ativacao' (ponderada wj xj)
                        let h = xj

                        let ajuste = e * taxaAjuste * f'u * h
                        wj + ajuste

                    let wiMap i wi =
                        let h'u = ponderada wi par.X |> ativacao'
                        let somatorio =
                            let wj j = m.J.[j]
                            let e j = erro.[j]
                            let f'u j = ponderada (wj j) xj |> ativacao'
                            let wji j = (wj j).[i]

                            let map j _ = e j * f'u j * wji j
                            m.J |> List.mapi map |> List.sum

                        let ajuste = taxaAjuste * h'u * somatorio * par.X
                        wi + ajuste

                    let j1 = m.J |> List.mapi wjMap
                    let i1 = m.I |> List.mapi wiMap

                    let m1 = { I = i1; J = j1 }

                    proximo tail m1
    
        //Decide se os pesos ainda devem ser atualizados (por número de épocas)
        let rec pesos m n =
            let w1 = proximo (dados.SelectPermutation() |> List.ofSeq) m

            if n < maxN then pesos w1 (n+1) else w1
        
        let vetorAleatorioFn n = 
            let f _ = Random.doubles n |> vector
            f
        
        let li = vetorAleatorioFn dados.Head.X.Count |> List.init numNeuronios 

        let lj = vetorAleatorioFn (numNeuronios + 1) |> List.init numSaidas 
        
        let m0 = { I = li; J = lj }

        //Inicia o treinamento
        pesos m0 0
    
    let precisao dados numSaidas ativacao ativacao' numNeuronios taxaAjuste  = 
        (dados: Par list) |> ignore
        let secoes = 5
        let tamanhoSecao = dados.Length / secoes

        let precisaoSecao n =
            let head = dados |> List.take (tamanhoSecao * n)
            let secao = dados |> List.skip (tamanhoSecao * n) |> List.take tamanhoSecao
            let tail = dados |> List.skip (tamanhoSecao * (n + 1))

            let treinamento = head @ tail
            let teste = secao

            let m = pesos treinamento numSaidas ativacao ativacao' numNeuronios taxaAjuste 
            let acertos = 
                teste |> 
                List.map (fun t -> resultado m t.X ativacao = t.Y) |>
                List.filter (fun r -> r) |>
                List.length |> float
            
            acertos / (float teste.Length)

        [0 .. (secoes - 1)] |> List.map precisaoSecao |> List.average

    let ajusteGridP dados numSaidas ativacao ativacao' neuronios taxas fPrecisao = 
        let combinacoes = List.allPairs neuronios taxas 
        
        printfn "Busca de parâmetros em grade\n"
        printfn "Neurônios X Taxas: %A x %A" neuronios taxas
        let map (neuronios, taxa) =
            let precisao = fPrecisao dados numSaidas ativacao ativacao' neuronios taxa 
            let mapping = { NumeroNeuronios = neuronios; TaxaAprendizado = taxa; Precisao = precisao }
            printfn "%A" mapping
            mapping
            
        combinacoes |> PSeq.map map |> PSeq.maxBy (fun r -> r.Precisao)
    
    let ajusteGrid dados numSaidas ativacao ativacao' neuronios taxas =
        ajusteGridP dados numSaidas ativacao ativacao' neuronios taxas precisao
    
    let realizacao dados classes ativacao ativacao' numNeuronios taxaAjuste =
        let numClasses = classes |> List.length        
        let confusao = DenseMatrix.zero numClasses numClasses
    
        let treinamento = 
            let n = dados |> List.length |> float |> (*) 0.8 |> int
            dados |> List.take n

        let teste = dados |> List.except treinamento

        let w = pesos treinamento numClasses ativacao ativacao' numNeuronios taxaAjuste 
        
        let iter par =
            let y = resultado w par.X ativacao
            let index = classes |> List.tryFindIndex (fun e -> e = y)

            match index with
                | Some i -> 
                    let j = classes |> List.findIndex (fun e -> e = par.Y)
                    confusao.[i, j] <- confusao.[i, j] + 1.0
                | None -> ()

        teste |> Seq.iter iter
        
        { TaxaAcerto = confusao.Diagonal().Sum() / float (teste |> Seq.length) ; Confusao = confusao; W = w }
    
    let algoritmo dados classes ativacao ativacao' neuronios taxas = 
        let numClasses = classes |> List.length

        sw.Start()
        let parametros = ajusteGrid dados numClasses ativacao ativacao' neuronios taxas
        sw.Stop()
        printfn "\nParametros escolhidos: \n%A \n(%A)\n" parametros sw.Elapsed

        sw.Restart()
        printf "Fazendo realizacoes... "
        
        let map _ = 
            realizacao (dados.SelectPermutation() |> List.ofSeq) classes ativacao ativacao' parametros.NumeroNeuronios parametros.TaxaAprendizado

        let realizacoes =
            [0 .. 20] |> PSeq.map map |> PSeq.toList
    
        let maior = 
            realizacoes |>
            List.maxBy (fun r -> r.TaxaAcerto)
        
        let media =
            realizacoes |>
            List.averageBy (fun r -> r.TaxaAcerto)

        let desvio = 
            realizacoes |>
            List.map (fun r -> r.TaxaAcerto) |>
            Statistics.StandardDeviation
        
        sw.Stop()
        printfn "%A\n" sw.Elapsed

        { Acuracia = media; DesvioPadrao = desvio; Melhor = maior; }
     
    let algoritmoCSV db classes colunas ativacao ativacao' neuronios taxas =
        (db : Runtime.CsvFile<CsvRow>) |> ignore
        (classes: Map<string, float Vector>) |> ignore

        let colunaClasse = colunas
        let parse (s: string) = 
            match s with
                | "?" -> 0.0
                | s -> s.Replace(".", ",") |> System.Double.Parse
        
        let minMax =
            let parseRow (row: CsvRow) = row.Columns|> Seq.take colunas |> Seq.map parse
            let valores = db.Rows |> Seq.collect parseRow
            let min = valores |> Seq.min
            let max = valores |> Seq.max

            (min, max)
        
        let (min, max) = minMax

        let normaliza x = normaliza x min max

        let normaliza s = parse s |> normaliza

        let parseRow (row: CsvRow) = row.Columns |> Seq.take colunas |> Seq.map normaliza |> List.ofSeq

        let mapRow (row: CsvRow) = { X = parseRow row |> vector; Y = classes.[row.[colunaClasse]] |> vector }
    
        let dados = db.Rows |> Seq.map mapRow |> List.ofSeq
        //let classes = classes.Values |> Seq.map (fun e -> vector e) |> List.ofSeq
        let classes = classes |> Map.toList |> List.map (fun (_, v) -> v)


        algoritmo dados classes ativacao ativacao' neuronios taxas

    let algoritmoIris () =
        printfn "Iris"
        let db = CsvFile.Load("iris.data").Cache()
        let classes = Map.ofList [("Iris-setosa", vector [1.0; 0.0; 0.0]); ("Iris-versicolor", vector [0.0; 1.0; 0.0]); ("Iris-virginica", vector [0.0; 0.0; 1.0])]

        let neuronios = [4 .. 10]
        let taxas = [0.1 .. 0.1 .. 0.5]

        algoritmoCSV db classes 4 sigmoide sigmoide' neuronios taxas

    let algoritmoColuna () =
        printfn "Coluna Terbreval"
        let db = CsvFile.Load("column_3C.dat", " ").Cache()
        let classes = Map.ofList [("DH", vector [1.0; 0.0; 0.0]); ("SL", vector [0.0; 1.0; 0.0]); "NO", (vector [0.0; 0.0; 1.0])]
        let taxas = [0.1 .. 0.1 .. 0.5]
        let neuronios = [4 .. 10]

        algoritmoCSV db classes 6 sigmoide sigmoide' neuronios taxas
    
    let classesMap list = 
        let num = list |> List.length
        let gen index n = 
            let head = List.init (index) (fun _ -> 0.0)
            let tail = List.init (num - head.Length - 1) (fun _ -> 0.0)
            let v = head @ [1.0] @ tail |> vector
            (n.ToString(), v)
        list |> List.mapi gen |> Map.ofList
        
    let algoritmoDermatologia () =
        printfn "Dermatologia"
        let db = CsvFile.Load("dermatology.data").Cache()
        
        let classes = classesMap [1..6]
        let taxas = [0.1]
        let neuronios = [7 .. 10]

        algoritmoCSV db classes 34 sigmoide sigmoide' neuronios taxas

    let algoritmoCancer () =
        printfn "Câncer de Mama"
        let db = CsvFile.Load("breast-cancer-wisconsin.data").Cache()
        
        let classes = classesMap[2; 4]
        let taxas = [0.1]
        let neuronios = [7 .. 10]

        algoritmoCSV db classes 10 sigmoide sigmoide' neuronios taxas
    
    let classesXor = [ vector [1.0; 0.0]; vector [0.0; 1.0]]
    let classesXorSeq = classesXor |> seq

    let dadosXor = 
        let range min max n =
            let range = max - min
            n * range + min
        
        let mapV (x, y) = 
            vector [x; y]

        let x1 = Random.doubles 50 |> Seq.map (range 0.0 0.5)
        let y1 = Random.doubles 50 |> Seq.map (range 0.0 0.5)
        let q1 = Seq.zip x1 y1 |> Seq.map mapV |> List.ofSeq

        let x2 = Random.doubles 50 |> Seq.map (range 0.5 1.0)
        let y2 = Random.doubles 50 |> Seq.map (range 0.0 0.5)
        let q2 = Seq.zip x2 y2 |> Seq.map mapV |> List.ofSeq

        let x3 = Random.doubles 50 |> Seq.map (range 0.0 0.5)
        let y3 = Random.doubles 50 |> Seq.map (range 0.5 1.0)
        let q3 = Seq.zip x3 y3 |> Seq.map mapV |> List.ofSeq
        
        let x4 = Random.doubles 50 |> Seq.map (range 0.5 1.0)
        let y4 = Random.doubles 50 |> Seq.map (range 0.5 1.0)
        let q4 = Seq.zip x4 y4 |> Seq.map mapV |> List.ofSeq

        let mapClass index c =
            {X = c; Y = classesXor.[index]}

        let classe1 = q1 @ q4 |> List.map (mapClass 0)
        let classe2 = q2 @ q3 |> List.map (mapClass 1)
        let dados = classe1 @ classe2
        dados
    
    let dadosXorSeq = dadosXor |> seq

    let algoritmoXor () =
        printfn "XOR"
        
        let neuronios = [8 .. 10]
        let taxas = [0.4 .. 0.1 .. 0.5]

        algoritmo dadosXor classesXor sigmoide sigmoide' neuronios taxas

    let algoritmoRegressao () =
        printfn "Regressão"
        
        let map n =
            let x = normaliza n 0.0 10.0
            let y = funcaoRegessao n
            { X = vector [x]; Y = vector[y] }

        let dados = [0.0 .. 0.02 .. 10.0] |> List.map map
        let neuronios = [4..20]
        let taxas = [0.1]

        let precisao dados numSaidas ativacao ativacao' numNeuronios taxaAjuste = 
            (dados: Par list) |> ignore
            let secoes = 5
            let tamanhoSecao = dados.Length / secoes

            let precisaoSecao n =
                let head = dados |> List.take (tamanhoSecao * n)
                let secao = dados |> List.skip (tamanhoSecao * n) |> List.take tamanhoSecao
                let tail = dados |> List.skip (tamanhoSecao * (n + 1))

                let treinamento = head @ tail
                let teste = secao

                let m = pesos treinamento numSaidas ativacao ativacao' numNeuronios taxaAjuste 
                let map t =
                    let erro = t.Y.[0] - (resultadoLinear m t.X).[0]
                    erro * erro
                
                let erro = 
                    teste |> 
                    List.map map |>
                    List.average |>
                    Math.Sqrt
                
                -erro

            let erros = [0 .. (secoes - 1)] |> List.map precisaoSecao

            erros |> List.average

        let realizacao dados numNeuronios taxaAjuste =
    
            let treinamento = 
                let n = dados |> List.length |> float |> (*) 0.8 |> int
                dados |> List.take n

            let teste = dados |> List.except treinamento

            let w = pesos treinamento 1 linear linear' numNeuronios taxaAjuste 
        
            let map par =
                let y = resultadoLinear w par.X
                let erro = par.Y.[0] - y.[0]
                erro * erro

            let erros = teste |> List.map map

            let rmse = erros |> List.average |> Math.Sqrt
        
            { RMSE = rmse ; W = w }
    
        let algoritmo = 
            printfn "Busca de parâmetros em grade\n"
            sw.Start()
            let parametros = ajusteGridP dados 1 linear linear' neuronios taxas precisao
            sw.Stop()
            printfn "\nParametros escolhidos: \n%A \n(%A)\n" parametros sw.Elapsed

            sw.Restart()
            printf "Fazendo realizacoes... "
        
            let map _ = 
                realizacao (dados.SelectPermutation() |> List.ofSeq) parametros.NumeroNeuronios parametros.TaxaAprendizado

            let realizacoes =
                [0 .. 20] |> PSeq.map map |> PSeq.toList
    
            let maior = 
                realizacoes |>
                List.maxBy (fun r -> r.RMSE)
        
            let media =
                realizacoes |>
                List.averageBy (fun r -> r.RMSE)

            let desvioPadrao = 
                realizacoes |> 
                List.map (fun r -> r.RMSE) |> 
                Statistics.StandardDeviation
        
            sw.Stop()
            printfn "%A\n" sw.Elapsed

            { RMSE = media; DesvioPadrao = desvioPadrao; Melhor = maior; }

        let range = [0.0 .. 0.001 .. 0.02] |> List.map(fun n -> vector [n] |> resultadoLinear algoritmo.Melhor.W |> vector)
        printfn "%A" range
        
        algoritmo