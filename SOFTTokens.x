{
module Main (main) where
}

%wrapper "monad"

$spaces = [\ \t]
$alpha = [a-zA-Z]
$digits = [0-9]
$alnum = [$alpha$digits]


@identifier = $alpha $alnum*

@comment = \#.*

@integer = $digits+

@boolean = (true) | (false)

@string = \"[^\"]*\"

:-

@integer    { mkL LInteger }
@boolean    { mkL LBoolean }
@string     { mkL LString }
