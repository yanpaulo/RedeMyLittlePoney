namespace RedeMyLittlePoney

open System
open FSharp.Data
open MathNet.Numerics
open MathNet.Numerics.Random
open MathNet.Numerics.LinearAlgebra
open System.Diagnostics

module Algoritmo =
    //Tipos
    type Par = { X: float Vector; Y: float Vector }
    type Modelo = { I: float Vector list; J: float Vector list }
    type Realizacao = { TaxaAcerto:float; Confusao: float Matrix; W: float Matrix }
    type ResultadoAlgoritmo = { Acuracia: float; Tempo: TimeSpan; Melhor: Realizacao; Dados: Par seq; }
    
    //Funções
    let sigmoide x = 
        1.0 /  (1.0 + Math.Pow(Math.E, -x))
    
    let ponderada w x =
        x .* w |> Vector.sum

    let saida w x =
        ponderada w x |> sigmoide

    let resultado m x =
        let xj = m.I |> List.map (fun i -> saida i x) |> vector
        m.J |> List.map (fun j -> saida j xj)

    //Próximo modelo para o vetor "treinamento"
    let pesos treinamento =
        (treinamento: Par list) |> ignore

        //Máximo de épocas
        let maxN = 500
        
        //Próximo modelo (função w(n+1))
        let rec proximo t m e = 
            match t with
                | [] -> (m, e)
                | par :: tail -> 
                    let e0 = e
                    let m1 = m

                    proximo tail m e
    
        //Decide se os pesos ainda devem ser atualizados (por número de épocas e ausência de erros)
        let rec pesos m n =
            let (w1, e1) = proximo (treinamento.SelectPermutation() |> List.ofSeq) m false
            if e1 && n < maxN  then pesos w1 (n+1) else w1
    
        let m0x = Random.doubles treinamento.Head.X.Count;
        let m0y = Random.doubles treinamento.Head.Y.Count;
        
        let m0 = { I = [ vector m0x; vector m0x]; J = [vector m0y; vector m0y] }

        //Inicia o treinamento
        pesos m0 0
