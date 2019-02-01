\documentclass{article}
\usepackage{minted}
\newenvironment{code}{\VerbatimEnvironment \begin{minted}{haskell}}{\end{minted}}
\begin{document}

\begin{code}
module Main where

main :: IO ()
main = putStrLn "Hello, Haskell!"
\end{code}

\end{document}
