module System.Concurrency

%default total

-- Futures based Concurrency and Parallelism

export
data Future : Type -> Type where [external]

%foreign "scheme:blodwen-make-future"
prim__makeFuture : {0 a : Type} -> (1 prog : PrimIO a) -> PrimIO (Future a)

%foreign "scheme:blodwen-await-future"
prim__awaitFuture : {0 a : Type} -> Future a -> PrimIO a

export
fork : (1 prog : IO a) -> IO (Future a)
fork act = fromPrim (prim__makeFuture (toPrim act))

export
await : Future a -> IO a
await f = fromPrim (prim__awaitFuture f)
