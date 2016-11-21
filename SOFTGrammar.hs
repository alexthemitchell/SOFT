{-# OPTIONS_GHC -w #-}
-- Output from SOFTGrammar.y
--  DO NOT MAKE CHANGES IN THIS FILE
--  Instead, edit SOFTGrammar.y and run:
--  happy SOFTGrammar.y
module SOFTGrammar where
import SOFTLexer
import SOFTEval
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.19.5

data HappyAbsSyn t4 t5
	= HappyTerminal (SOFTLexer.Token)
	| HappyErrorToken Int
	| HappyAbsSyn4 t4
	| HappyAbsSyn5 t5

action_0 (6) = happyShift action_2
action_0 (8) = happyShift action_5
action_0 (9) = happyShift action_6
action_0 (23) = happyShift action_7
action_0 (25) = happyShift action_8
action_0 (32) = happyShift action_9
action_0 (33) = happyShift action_10
action_0 (4) = happyGoto action_3
action_0 (5) = happyGoto action_4
action_0 _ = happyFail

action_1 (6) = happyShift action_2
action_1 _ = happyFail

action_2 (7) = happyShift action_25
action_2 _ = happyFail

action_3 (34) = happyAccept
action_3 _ = happyFail

action_4 (12) = happyShift action_13
action_4 (13) = happyShift action_14
action_4 (14) = happyShift action_15
action_4 (15) = happyShift action_16
action_4 (16) = happyShift action_17
action_4 (17) = happyShift action_18
action_4 (18) = happyShift action_19
action_4 (19) = happyShift action_20
action_4 (20) = happyShift action_21
action_4 (21) = happyShift action_22
action_4 (22) = happyShift action_23
action_4 (24) = happyShift action_24
action_4 _ = happyReduce_2

action_5 _ = happyReduce_4

action_6 _ = happyReduce_5

action_7 (8) = happyShift action_5
action_7 (9) = happyShift action_6
action_7 (23) = happyShift action_7
action_7 (25) = happyShift action_8
action_7 (32) = happyShift action_9
action_7 (33) = happyShift action_10
action_7 (5) = happyGoto action_12
action_7 _ = happyFail

action_8 (6) = happyShift action_2
action_8 (8) = happyShift action_5
action_8 (9) = happyShift action_6
action_8 (23) = happyShift action_7
action_8 (25) = happyShift action_8
action_8 (32) = happyShift action_9
action_8 (33) = happyShift action_10
action_8 (4) = happyGoto action_11
action_8 (5) = happyGoto action_4
action_8 _ = happyFail

action_9 _ = happyReduce_3

action_10 _ = happyReduce_6

action_11 (26) = happyShift action_39
action_11 _ = happyFail

action_12 (12) = happyShift action_13
action_12 (13) = happyShift action_14
action_12 (14) = happyShift action_15
action_12 (15) = happyShift action_16
action_12 (16) = happyShift action_17
action_12 (17) = happyShift action_18
action_12 (18) = happyShift action_19
action_12 (19) = happyShift action_20
action_12 (20) = happyShift action_21
action_12 (21) = happyShift action_22
action_12 (22) = happyShift action_23
action_12 (24) = happyShift action_24
action_12 _ = happyReduce_19

action_13 (8) = happyShift action_5
action_13 (9) = happyShift action_6
action_13 (23) = happyShift action_7
action_13 (25) = happyShift action_8
action_13 (32) = happyShift action_9
action_13 (33) = happyShift action_10
action_13 (5) = happyGoto action_38
action_13 _ = happyFail

action_14 (8) = happyShift action_5
action_14 (9) = happyShift action_6
action_14 (23) = happyShift action_7
action_14 (25) = happyShift action_8
action_14 (32) = happyShift action_9
action_14 (33) = happyShift action_10
action_14 (5) = happyGoto action_37
action_14 _ = happyFail

action_15 (8) = happyShift action_5
action_15 (9) = happyShift action_6
action_15 (23) = happyShift action_7
action_15 (25) = happyShift action_8
action_15 (32) = happyShift action_9
action_15 (33) = happyShift action_10
action_15 (5) = happyGoto action_36
action_15 _ = happyFail

action_16 (8) = happyShift action_5
action_16 (9) = happyShift action_6
action_16 (23) = happyShift action_7
action_16 (25) = happyShift action_8
action_16 (32) = happyShift action_9
action_16 (33) = happyShift action_10
action_16 (5) = happyGoto action_35
action_16 _ = happyFail

action_17 (8) = happyShift action_5
action_17 (9) = happyShift action_6
action_17 (23) = happyShift action_7
action_17 (25) = happyShift action_8
action_17 (32) = happyShift action_9
action_17 (33) = happyShift action_10
action_17 (5) = happyGoto action_34
action_17 _ = happyFail

action_18 (8) = happyShift action_5
action_18 (9) = happyShift action_6
action_18 (23) = happyShift action_7
action_18 (25) = happyShift action_8
action_18 (32) = happyShift action_9
action_18 (33) = happyShift action_10
action_18 (5) = happyGoto action_33
action_18 _ = happyFail

action_19 (8) = happyShift action_5
action_19 (9) = happyShift action_6
action_19 (23) = happyShift action_7
action_19 (25) = happyShift action_8
action_19 (32) = happyShift action_9
action_19 (33) = happyShift action_10
action_19 (5) = happyGoto action_32
action_19 _ = happyFail

action_20 (8) = happyShift action_5
action_20 (9) = happyShift action_6
action_20 (23) = happyShift action_7
action_20 (25) = happyShift action_8
action_20 (32) = happyShift action_9
action_20 (33) = happyShift action_10
action_20 (5) = happyGoto action_31
action_20 _ = happyFail

action_21 (8) = happyShift action_5
action_21 (9) = happyShift action_6
action_21 (23) = happyShift action_7
action_21 (25) = happyShift action_8
action_21 (32) = happyShift action_9
action_21 (33) = happyShift action_10
action_21 (5) = happyGoto action_30
action_21 _ = happyFail

action_22 (8) = happyShift action_5
action_22 (9) = happyShift action_6
action_22 (23) = happyShift action_7
action_22 (25) = happyShift action_8
action_22 (32) = happyShift action_9
action_22 (33) = happyShift action_10
action_22 (5) = happyGoto action_29
action_22 _ = happyFail

action_23 (8) = happyShift action_5
action_23 (9) = happyShift action_6
action_23 (23) = happyShift action_7
action_23 (25) = happyShift action_8
action_23 (32) = happyShift action_9
action_23 (33) = happyShift action_10
action_23 (5) = happyGoto action_28
action_23 _ = happyFail

action_24 (8) = happyShift action_5
action_24 (9) = happyShift action_6
action_24 (23) = happyShift action_7
action_24 (25) = happyShift action_8
action_24 (32) = happyShift action_9
action_24 (33) = happyShift action_10
action_24 (5) = happyGoto action_27
action_24 _ = happyFail

action_25 (11) = happyShift action_26
action_25 _ = happyFail

action_26 (6) = happyShift action_2
action_26 (8) = happyShift action_5
action_26 (9) = happyShift action_6
action_26 (23) = happyShift action_7
action_26 (25) = happyShift action_8
action_26 (32) = happyShift action_9
action_26 (33) = happyShift action_10
action_26 (4) = happyGoto action_40
action_26 (5) = happyGoto action_4
action_26 _ = happyFail

action_27 (12) = happyShift action_13
action_27 (13) = happyShift action_14
action_27 (14) = happyShift action_15
action_27 (15) = happyShift action_16
action_27 (16) = happyShift action_17
action_27 (17) = happyShift action_18
action_27 (18) = happyShift action_19
action_27 (19) = happyShift action_20
action_27 (20) = happyShift action_21
action_27 (21) = happyShift action_22
action_27 (22) = happyShift action_23
action_27 (24) = happyShift action_24
action_27 _ = happyReduce_10

action_28 (12) = happyShift action_13
action_28 (13) = happyShift action_14
action_28 (14) = happyShift action_15
action_28 (15) = happyShift action_16
action_28 (16) = happyShift action_17
action_28 (17) = happyShift action_18
action_28 (18) = happyShift action_19
action_28 (19) = happyShift action_20
action_28 (20) = happyShift action_21
action_28 (21) = happyShift action_22
action_28 (22) = happyShift action_23
action_28 (24) = happyShift action_24
action_28 _ = happyReduce_18

action_29 (12) = happyShift action_13
action_29 (13) = happyShift action_14
action_29 (14) = happyShift action_15
action_29 (15) = happyShift action_16
action_29 (16) = happyShift action_17
action_29 (17) = happyShift action_18
action_29 (18) = happyShift action_19
action_29 (19) = happyShift action_20
action_29 (20) = happyShift action_21
action_29 (21) = happyShift action_22
action_29 (22) = happyShift action_23
action_29 (24) = happyShift action_24
action_29 _ = happyReduce_17

action_30 (12) = happyShift action_13
action_30 (13) = happyShift action_14
action_30 (14) = happyShift action_15
action_30 (15) = happyShift action_16
action_30 (16) = happyShift action_17
action_30 (17) = happyShift action_18
action_30 (18) = happyShift action_19
action_30 (19) = happyShift action_20
action_30 (20) = happyShift action_21
action_30 (21) = happyShift action_22
action_30 (22) = happyShift action_23
action_30 (24) = happyShift action_24
action_30 _ = happyReduce_15

action_31 (12) = happyShift action_13
action_31 (13) = happyShift action_14
action_31 (14) = happyShift action_15
action_31 (15) = happyShift action_16
action_31 (16) = happyShift action_17
action_31 (17) = happyShift action_18
action_31 (18) = happyShift action_19
action_31 (19) = happyShift action_20
action_31 (20) = happyShift action_21
action_31 (21) = happyShift action_22
action_31 (22) = happyShift action_23
action_31 (24) = happyShift action_24
action_31 _ = happyReduce_16

action_32 (12) = happyShift action_13
action_32 (13) = happyShift action_14
action_32 (14) = happyShift action_15
action_32 (15) = happyShift action_16
action_32 (16) = happyShift action_17
action_32 (17) = happyShift action_18
action_32 (18) = happyShift action_19
action_32 (19) = happyShift action_20
action_32 (20) = happyShift action_21
action_32 (21) = happyShift action_22
action_32 (22) = happyShift action_23
action_32 (24) = happyShift action_24
action_32 _ = happyReduce_14

action_33 (12) = happyShift action_13
action_33 (13) = happyShift action_14
action_33 (14) = happyShift action_15
action_33 (15) = happyShift action_16
action_33 (16) = happyShift action_17
action_33 (17) = happyShift action_18
action_33 (18) = happyShift action_19
action_33 (19) = happyShift action_20
action_33 (20) = happyShift action_21
action_33 (21) = happyShift action_22
action_33 (22) = happyShift action_23
action_33 (24) = happyShift action_24
action_33 _ = happyReduce_13

action_34 (12) = happyShift action_13
action_34 (13) = happyShift action_14
action_34 (14) = happyShift action_15
action_34 (15) = happyShift action_16
action_34 (16) = happyShift action_17
action_34 (17) = happyShift action_18
action_34 (18) = happyShift action_19
action_34 (19) = happyShift action_20
action_34 (20) = happyShift action_21
action_34 (21) = happyShift action_22
action_34 (22) = happyShift action_23
action_34 (24) = happyShift action_24
action_34 _ = happyReduce_11

action_35 (12) = happyShift action_13
action_35 (13) = happyShift action_14
action_35 (14) = happyShift action_15
action_35 (15) = happyShift action_16
action_35 (16) = happyShift action_17
action_35 (17) = happyShift action_18
action_35 (18) = happyShift action_19
action_35 (19) = happyShift action_20
action_35 (20) = happyShift action_21
action_35 (21) = happyShift action_22
action_35 (22) = happyShift action_23
action_35 (24) = happyShift action_24
action_35 _ = happyReduce_9

action_36 (12) = happyShift action_13
action_36 (13) = happyShift action_14
action_36 (14) = happyShift action_15
action_36 (15) = happyShift action_16
action_36 (16) = happyShift action_17
action_36 (17) = happyShift action_18
action_36 (18) = happyShift action_19
action_36 (19) = happyShift action_20
action_36 (20) = happyShift action_21
action_36 (21) = happyShift action_22
action_36 (22) = happyShift action_23
action_36 (24) = happyShift action_24
action_36 _ = happyReduce_8

action_37 (12) = happyShift action_13
action_37 (13) = happyShift action_14
action_37 (14) = happyShift action_15
action_37 (15) = happyShift action_16
action_37 (16) = happyShift action_17
action_37 (17) = happyShift action_18
action_37 (18) = happyShift action_19
action_37 (19) = happyShift action_20
action_37 (20) = happyShift action_21
action_37 (21) = happyShift action_22
action_37 (22) = happyShift action_23
action_37 (24) = happyShift action_24
action_37 _ = happyReduce_7

action_38 (12) = happyShift action_13
action_38 (13) = happyShift action_14
action_38 (14) = happyShift action_15
action_38 (15) = happyShift action_16
action_38 (16) = happyShift action_17
action_38 (17) = happyShift action_18
action_38 (18) = happyShift action_19
action_38 (19) = happyShift action_20
action_38 (20) = happyShift action_21
action_38 (21) = happyShift action_22
action_38 (22) = happyShift action_23
action_38 (24) = happyShift action_24
action_38 _ = happyReduce_12

action_39 _ = happyReduce_20

action_40 _ = happyReduce_1

happyReduce_1 = happyReduce 4 4 happyReduction_1
happyReduction_1 (_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (ENil
	) `HappyStk` happyRest

happyReduce_2 = happySpecReduce_1  4 happyReduction_2
happyReduction_2 (HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn4
		 (evaluate happy_var_1
	)
happyReduction_2 _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_1  5 happyReduction_3
happyReduction_3 (HappyTerminal (TokenInt happy_var_1))
	 =  HappyAbsSyn5
		 (EInt happy_var_1
	)
happyReduction_3 _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_1  5 happyReduction_4
happyReduction_4 _
	 =  HappyAbsSyn5
		 (True
	)

happyReduce_5 = happySpecReduce_1  5 happyReduction_5
happyReduction_5 _
	 =  HappyAbsSyn5
		 (False
	)

happyReduce_6 = happySpecReduce_1  5 happyReduction_6
happyReduction_6 (HappyTerminal (TokenStr happy_var_1))
	 =  HappyAbsSyn5
		 (EStr happy_var_1
	)
happyReduction_6 _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_3  5 happyReduction_7
happyReduction_7 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (EAdd happy_var_1 happy_var_3
	)
happyReduction_7 _ _ _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_3  5 happyReduction_8
happyReduction_8 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (ESub happy_var_1 happy_var_3
	)
happyReduction_8 _ _ _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_3  5 happyReduction_9
happyReduction_9 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (EMul happy_var_1 happy_var_3
	)
happyReduction_9 _ _ _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_3  5 happyReduction_10
happyReduction_10 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (EDiv happy_var_1 happy_var_3
	)
happyReduction_10 _ _ _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_3  5 happyReduction_11
happyReduction_11 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (EMod happy_var_1 happy_var_3
	)
happyReduction_11 _ _ _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_3  5 happyReduction_12
happyReduction_12 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (EEql happy_var_1 happy_var_3
	)
happyReduction_12 _ _ _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_3  5 happyReduction_13
happyReduction_13 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (ELtn happy_var_1 happy_var_3
	)
happyReduction_13 _ _ _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_3  5 happyReduction_14
happyReduction_14 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (EGtn happy_var_1 happy_var_3
	)
happyReduction_14 _ _ _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_3  5 happyReduction_15
happyReduction_15 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (EGeq happy_var_1 happy_var_3
	)
happyReduction_15 _ _ _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_3  5 happyReduction_16
happyReduction_16 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (ELeq happy_var_1 happy_var_3
	)
happyReduction_16 _ _ _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_3  5 happyReduction_17
happyReduction_17 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (EAnd happy_var_1 happy_var_3
	)
happyReduction_17 _ _ _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_3  5 happyReduction_18
happyReduction_18 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (EAnd happy_var_1 happy_var_3
	)
happyReduction_18 _ _ _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_2  5 happyReduction_19
happyReduction_19 (HappyAbsSyn5  happy_var_2)
	_
	 =  HappyAbsSyn5
		 (ENot happy_var_2
	)
happyReduction_19 _ _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_3  5 happyReduction_20
happyReduction_20 _
	(HappyAbsSyn4  happy_var_2)
	_
	 =  HappyAbsSyn5
		 (EClos happy_var_2
	)
happyReduction_20 _ _ _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 34 34 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TokenLet -> cont 6;
	TokenVar happy_dollar_dollar -> cont 7;
	TokenTrue -> cont 8;
	TokenFalse -> cont 9;
	TokenComment -> cont 10;
	TokenEqual -> cont 11;
	TokenDoubleEqual -> cont 12;
	TokenPlus -> cont 13;
	TokenMinus -> cont 14;
	TokenAsterisk -> cont 15;
	TokenMod -> cont 16;
	TokenLT -> cont 17;
	TokenGT -> cont 18;
	TokenLEQ -> cont 19;
	TokenGEQ -> cont 20;
	TokenAnd -> cont 21;
	TokenOr -> cont 22;
	TokenNot -> cont 23;
	TokenFSlash -> cont 24;
	TokenLParen -> cont 25;
	TokenRParen -> cont 26;
	TokenLBrace -> cont 27;
	TokenRBrace -> cont 28;
	TokenLSqBrkt -> cont 29;
	TokenRSqBrkt -> cont 30;
	TokenQuotation -> cont 31;
	TokenInt happy_dollar_dollar -> cont 32;
	TokenStr happy_dollar_dollar -> cont 33;
	_ -> happyError' (tk:tks)
	}

happyError_ 34 tk tks = happyError' tks
happyError_ _ tk tks = happyError' (tk:tks)

newtype HappyIdentity a = HappyIdentity a
happyIdentity = HappyIdentity
happyRunIdentity (HappyIdentity a) = a

instance Functor HappyIdentity where
    fmap f (HappyIdentity a) = HappyIdentity (f a)

instance Applicative HappyIdentity where
    pure  = return
    (<*>) = ap
instance Monad HappyIdentity where
    return = HappyIdentity
    (HappyIdentity p) >>= q = q p

happyThen :: () => HappyIdentity a -> (a -> HappyIdentity b) -> HappyIdentity b
happyThen = (>>=)
happyReturn :: () => a -> HappyIdentity a
happyReturn = (return)
happyThen1 m k tks = (>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> HappyIdentity a
happyReturn1 = \a tks -> (return) a
happyError' :: () => [(SOFTLexer.Token)] -> HappyIdentity a
happyError' = HappyIdentity . parseError

parse tks = happyRunIdentity happySomeParser where
  happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq



{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command-line>" #-}
{-# LINE 8 "<command-line>" #-}
# 1 "/usr/include/stdc-predef.h" 1 3 4

# 17 "/usr/include/stdc-predef.h" 3 4










































{-# LINE 8 "<command-line>" #-}
{-# LINE 1 "/usr/lib/ghc/include/ghcversion.h" #-}

















{-# LINE 8 "<command-line>" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 

{-# LINE 13 "templates/GenericTemplate.hs" #-}

{-# LINE 46 "templates/GenericTemplate.hs" #-}








{-# LINE 67 "templates/GenericTemplate.hs" #-}

{-# LINE 77 "templates/GenericTemplate.hs" #-}

{-# LINE 86 "templates/GenericTemplate.hs" #-}

infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is (1), it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
         (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action

{-# LINE 155 "templates/GenericTemplate.hs" #-}

-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Int ->                    -- token number
         Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k - ((1) :: Int)) sts of
         sts1@(((st1@(HappyState (action))):(_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
         let drop_stk = happyDropStk k stk





             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n - ((1) :: Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - ((1)::Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction

{-# LINE 256 "templates/GenericTemplate.hs" #-}
happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery ((1) is the error token)

-- parse error if we are in recovery and we fail again
happyFail (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--      trace "failing" $ 
        happyError_ i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  (1) tk old_st (((HappyState (action))):(sts)) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        action (1) (1) tk (HappyState (action)) sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail  i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
        action (1) (1) tk (HappyState (action)) sts ( (HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.

{-# LINE 322 "templates/GenericTemplate.hs" #-}
{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
