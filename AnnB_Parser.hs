{-# OPTIONS_GHC -w #-}
module AnnB_Parser where
import AnnB_Lexer
import Types
import ParserModel
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.20.1.1

data HappyAbsSyn 
	= HappyTerminal (Token)
	| HappyErrorToken Prelude.Int
	| HappyAbsSyn4 (AnnB)
	| HappyAbsSyn5 ([Def])
	| HappyAbsSyn6 ([SigmaDef])
	| HappyAbsSyn7 ([String])
	| HappyAbsSyn8 ([AgentDef])
	| HappyAbsSyn9 ([Msg])
	| HappyAbsSyn10 (Msg)
	| HappyAbsSyn11 ([Knowledge])
	| HappyAbsSyn12 ([CellDef])
	| HappyAbsSyn13 (Action)
	| HappyAbsSyn14 ([Var])
	| HappyAbsSyn15 (Mode)
	| HappyAbsSyn16 (Formula Msg)
	| HappyAbsSyn17 (PProcess)

{- to allow type-synonyms as our monads (likely
 - with explicitly-specified bind and return)
 - in Haskell98, it seems that with
 - /type M a = .../, then /(HappyReduction M)/
 - is not allowed.  But Happy is a
 - code-generator that can just substitute it.
type HappyReduction m = 
	   Prelude.Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> m HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> m HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> m HappyAbsSyn
-}

action_0,
 action_1,
 action_2,
 action_3,
 action_4,
 action_5,
 action_6,
 action_7,
 action_8,
 action_9,
 action_10,
 action_11,
 action_12,
 action_13,
 action_14,
 action_15,
 action_16,
 action_17,
 action_18,
 action_19,
 action_20,
 action_21,
 action_22,
 action_23,
 action_24,
 action_25,
 action_26,
 action_27,
 action_28,
 action_29,
 action_30,
 action_31,
 action_32,
 action_33,
 action_34,
 action_35,
 action_36,
 action_37,
 action_38,
 action_39,
 action_40,
 action_41,
 action_42,
 action_43,
 action_44,
 action_45,
 action_46,
 action_47,
 action_48,
 action_49,
 action_50,
 action_51,
 action_52,
 action_53,
 action_54,
 action_55,
 action_56,
 action_57,
 action_58,
 action_59,
 action_60,
 action_61,
 action_62,
 action_63,
 action_64,
 action_65,
 action_66,
 action_67,
 action_68,
 action_69,
 action_70,
 action_71,
 action_72,
 action_73,
 action_74,
 action_75,
 action_76,
 action_77,
 action_78,
 action_79,
 action_80,
 action_81,
 action_82,
 action_83,
 action_84,
 action_85,
 action_86,
 action_87,
 action_88,
 action_89,
 action_90,
 action_91,
 action_92,
 action_93,
 action_94,
 action_95,
 action_96,
 action_97,
 action_98,
 action_99,
 action_100,
 action_101,
 action_102,
 action_103,
 action_104,
 action_105,
 action_106,
 action_107,
 action_108,
 action_109,
 action_110,
 action_111,
 action_112,
 action_113,
 action_114,
 action_115 :: () => Prelude.Int -> ({-HappyReduction (HappyIdentity) = -}
	   Prelude.Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (HappyIdentity) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (HappyIdentity) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (HappyIdentity) HappyAbsSyn)

happyReduce_1,
 happyReduce_2,
 happyReduce_3,
 happyReduce_4,
 happyReduce_5,
 happyReduce_6,
 happyReduce_7,
 happyReduce_8,
 happyReduce_9,
 happyReduce_10,
 happyReduce_11,
 happyReduce_12,
 happyReduce_13,
 happyReduce_14,
 happyReduce_15,
 happyReduce_16,
 happyReduce_17,
 happyReduce_18,
 happyReduce_19,
 happyReduce_20,
 happyReduce_21,
 happyReduce_22,
 happyReduce_23,
 happyReduce_24,
 happyReduce_25,
 happyReduce_26,
 happyReduce_27,
 happyReduce_28,
 happyReduce_29,
 happyReduce_30,
 happyReduce_31,
 happyReduce_32,
 happyReduce_33,
 happyReduce_34,
 happyReduce_35,
 happyReduce_36,
 happyReduce_37,
 happyReduce_38 :: () => ({-HappyReduction (HappyIdentity) = -}
	   Prelude.Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (HappyIdentity) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (HappyIdentity) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (HappyIdentity) HappyAbsSyn)

happyExpList :: Happy_Data_Array.Array Prelude.Int Prelude.Int
happyExpList = Happy_Data_Array.listArray (0,188) ([0,0,2048,0,0,0,16,0,2048,0,0,0,0,0,0,128,0,0,0,0,512,0,32768,0,0,0,1,0,0,0,0,32,0,0,4,0,16384,0,0,0,4096,0,0,8192,0,64,0,32768,0,0,32768,0,0,0,256,0,2,0,0,32768,0,1024,0,0,0,8,4096,0,0,16,0,0,0,0,0,0,0,0,0,128,32768,0,0,0,0,0,512,0,2,0,0,0,2,0,16,0,0,16,0,0,8192,0,32,0,64,0,0,0,128,0,0,0,0,0,0,8,0,0,0,0,0,0,0,0,0,0,0,512,0,8192,0,0,16384,0,0,0,32768,0,0,0,1,0,0,2048,0,0,0,2048,0,0,32768,0,0,0,16,0,0,8192,0,0,0,0,0,0,0,0,32768,0,0,4,0,0,2,0,0,0,4,0,0,0,0,0,0,2,0,0,0,0,0,0,1,0,0,0,4,0,0,16384,0,0,0,0,1024,0,1024,8,0,6144,8576,2,0,64,0,0,0,32,0,0,64,0,0,32768,0,0,0,2048,0,0,0,512,0,0,3072,0,45056,0,32,0,0,0,64,0,0,0,8,0,0,0,0,0,0,0,0,0,1,0,44,1024,0,0,0,0,0,0,0,16,0,0,0,8,0,0,0,1,10,0,0,0,0,0,0,0,256,0,11264,0,2,0,0,1024,0,0,0,0,0,0,0,0,0,5,0,4096,0,16384,0,0,0,0,0,0,0,0,0,0,2048,0,0,0,256,0,0,2048,0,24576,1,16,0,704,0,256,0,0,0,1,0,0,0,0,0,1024,0,0,0,2,0,0,2048,0,0,0,0,0,0,0,0,0,0,0,512,0,16384,0,0,0,0,32,0,0,0,0,0,0,0,0,0,4,0,0,0,0,0,0,2048,0,0,8192,0,0,0,0,2,0,0,2,0,0,0,8192,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_parseAnnB","annbspec","defs","sigmaDefs","consts","agents","messages","message","knowledges","cells","actions","vars","mode","formula","process","const","var","ident","int","\"(\"","\")\"","\"{\"","\"}\"","\":\"","\",\"","\"[\"","\"]\"","\"<>\"","\"*\"","\":=\"","\"=\"","\"/\"","\"->\"","\"if\"","\"then\"","\"else\"","\"end\"","\"new\"","\"in\"","\"dishonest\"","\"private\"","\"Protocol\"","\"Sigma0\"","\"Sigma\"","\"Cells\"","\"Agents\"","\"Knowledge\"","\"Actions\"","\"Bound\"","\"true\"","\"false\"","\"and\"","\"not\"","\"or\"","%eof"]
        bit_start = st Prelude.* 57
        bit_end = (st Prelude.+ 1) Prelude.* 57
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..56]
        token_strs_expected = Prelude.concatMap f bits_indexed
        f (Prelude.False, _) = []
        f (Prelude.True, nr) = [token_strs Prelude.!! nr]

action_0 (44) = happyShift action_2
action_0 (4) = happyGoto action_3
action_0 _ = happyFail (happyExpListPerState 0)

action_1 (44) = happyShift action_2
action_1 _ = happyFail (happyExpListPerState 1)

action_2 (26) = happyShift action_4
action_2 _ = happyFail (happyExpListPerState 2)

action_3 (57) = happyAccept
action_3 _ = happyFail (happyExpListPerState 3)

action_4 (20) = happyShift action_5
action_4 _ = happyFail (happyExpListPerState 4)

action_5 (45) = happyShift action_6
action_5 _ = happyFail (happyExpListPerState 5)

action_6 (26) = happyShift action_7
action_6 _ = happyFail (happyExpListPerState 6)

action_7 (18) = happyShift action_9
action_7 (5) = happyGoto action_8
action_7 _ = happyFail (happyExpListPerState 7)

action_8 (46) = happyShift action_11
action_8 _ = happyFail (happyExpListPerState 8)

action_9 (34) = happyShift action_10
action_9 _ = happyFail (happyExpListPerState 9)

action_10 (21) = happyShift action_13
action_10 _ = happyFail (happyExpListPerState 10)

action_11 (26) = happyShift action_12
action_11 _ = happyFail (happyExpListPerState 11)

action_12 (18) = happyShift action_9
action_12 (43) = happyShift action_17
action_12 (5) = happyGoto action_15
action_12 (6) = happyGoto action_16
action_12 _ = happyReduce_6

action_13 (27) = happyShift action_14
action_13 _ = happyReduce_3

action_14 (18) = happyShift action_9
action_14 (5) = happyGoto action_21
action_14 _ = happyFail (happyExpListPerState 14)

action_15 (18) = happyShift action_9
action_15 (43) = happyShift action_17
action_15 (5) = happyGoto action_15
action_15 (6) = happyGoto action_20
action_15 _ = happyReduce_6

action_16 (48) = happyShift action_19
action_16 _ = happyFail (happyExpListPerState 16)

action_17 (18) = happyShift action_9
action_17 (5) = happyGoto action_18
action_17 _ = happyFail (happyExpListPerState 17)

action_18 (18) = happyShift action_9
action_18 (43) = happyShift action_17
action_18 (5) = happyGoto action_15
action_18 (6) = happyGoto action_23
action_18 _ = happyReduce_6

action_19 (26) = happyShift action_22
action_19 _ = happyFail (happyExpListPerState 19)

action_20 _ = happyReduce_4

action_21 _ = happyReduce_2

action_22 (18) = happyShift action_26
action_22 (42) = happyShift action_27
action_22 (7) = happyGoto action_24
action_22 (8) = happyGoto action_25
action_22 _ = happyReduce_11

action_23 _ = happyReduce_5

action_24 (18) = happyShift action_26
action_24 (42) = happyShift action_27
action_24 (7) = happyGoto action_24
action_24 (8) = happyGoto action_31
action_24 _ = happyReduce_11

action_25 (49) = happyShift action_30
action_25 _ = happyFail (happyExpListPerState 25)

action_26 (27) = happyShift action_29
action_26 _ = happyReduce_8

action_27 (18) = happyShift action_26
action_27 (7) = happyGoto action_28
action_27 _ = happyFail (happyExpListPerState 27)

action_28 (18) = happyShift action_26
action_28 (42) = happyShift action_27
action_28 (7) = happyGoto action_24
action_28 (8) = happyGoto action_34
action_28 _ = happyReduce_11

action_29 (18) = happyShift action_26
action_29 (7) = happyGoto action_33
action_29 _ = happyFail (happyExpListPerState 29)

action_30 (26) = happyShift action_32
action_30 _ = happyFail (happyExpListPerState 30)

action_31 _ = happyReduce_9

action_32 (20) = happyShift action_36
action_32 (11) = happyGoto action_35
action_32 _ = happyFail (happyExpListPerState 32)

action_33 _ = happyReduce_7

action_34 _ = happyReduce_10

action_35 (47) = happyShift action_38
action_35 _ = happyFail (happyExpListPerState 35)

action_36 (26) = happyShift action_37
action_36 _ = happyFail (happyExpListPerState 36)

action_37 (18) = happyShift action_42
action_37 (9) = happyGoto action_40
action_37 (10) = happyGoto action_41
action_37 _ = happyFail (happyExpListPerState 37)

action_38 (26) = happyShift action_39
action_38 _ = happyFail (happyExpListPerState 38)

action_39 (18) = happyShift action_47
action_39 (12) = happyGoto action_46
action_39 _ = happyFail (happyExpListPerState 39)

action_40 (20) = happyShift action_36
action_40 (11) = happyGoto action_45
action_40 _ = happyReduce_17

action_41 (27) = happyShift action_44
action_41 _ = happyReduce_13

action_42 (22) = happyShift action_43
action_42 _ = happyReduce_15

action_43 (18) = happyShift action_42
action_43 (9) = happyGoto action_51
action_43 (10) = happyGoto action_41
action_43 _ = happyFail (happyExpListPerState 43)

action_44 (18) = happyShift action_42
action_44 (9) = happyGoto action_50
action_44 (10) = happyGoto action_41
action_44 _ = happyFail (happyExpListPerState 44)

action_45 _ = happyReduce_16

action_46 (50) = happyShift action_49
action_46 _ = happyFail (happyExpListPerState 46)

action_47 (28) = happyShift action_48
action_47 _ = happyFail (happyExpListPerState 47)

action_48 (18) = happyShift action_42
action_48 (10) = happyGoto action_54
action_48 _ = happyFail (happyExpListPerState 48)

action_49 (26) = happyShift action_53
action_49 _ = happyFail (happyExpListPerState 49)

action_50 _ = happyReduce_12

action_51 (23) = happyShift action_52
action_51 _ = happyFail (happyExpListPerState 51)

action_52 _ = happyReduce_14

action_53 (20) = happyShift action_57
action_53 (13) = happyGoto action_56
action_53 _ = happyReduce_22

action_54 (29) = happyShift action_55
action_54 _ = happyFail (happyExpListPerState 54)

action_55 (32) = happyShift action_61
action_55 _ = happyFail (happyExpListPerState 55)

action_56 (51) = happyShift action_60
action_56 _ = happyFail (happyExpListPerState 56)

action_57 (26) = happyShift action_58
action_57 (35) = happyShift action_59
action_57 _ = happyFail (happyExpListPerState 57)

action_58 (18) = happyShift action_67
action_58 (19) = happyShift action_68
action_58 (30) = happyShift action_69
action_58 (31) = happyShift action_70
action_58 (36) = happyShift action_71
action_58 (40) = happyShift action_72
action_58 (15) = happyGoto action_65
action_58 (17) = happyGoto action_66
action_58 _ = happyFail (happyExpListPerState 58)

action_59 (20) = happyShift action_64
action_59 _ = happyFail (happyExpListPerState 59)

action_60 (26) = happyShift action_63
action_60 _ = happyFail (happyExpListPerState 60)

action_61 (18) = happyShift action_42
action_61 (10) = happyGoto action_62
action_61 _ = happyFail (happyExpListPerState 61)

action_62 (18) = happyShift action_47
action_62 (12) = happyGoto action_87
action_62 _ = happyReduce_19

action_63 (21) = happyShift action_86
action_63 _ = happyFail (happyExpListPerState 63)

action_64 (26) = happyShift action_85
action_64 _ = happyFail (happyExpListPerState 64)

action_65 (18) = happyShift action_42
action_65 (19) = happyShift action_84
action_65 (52) = happyShift action_77
action_65 (53) = happyShift action_78
action_65 (55) = happyShift action_79
action_65 (10) = happyGoto action_75
action_65 (16) = happyGoto action_83
action_65 _ = happyFail (happyExpListPerState 65)

action_66 (20) = happyShift action_57
action_66 (13) = happyGoto action_82
action_66 _ = happyReduce_22

action_67 (28) = happyShift action_81
action_67 _ = happyFail (happyExpListPerState 67)

action_68 (32) = happyShift action_80
action_68 _ = happyFail (happyExpListPerState 68)

action_69 _ = happyReduce_26

action_70 _ = happyReduce_25

action_71 (18) = happyShift action_42
action_71 (52) = happyShift action_77
action_71 (53) = happyShift action_78
action_71 (55) = happyShift action_79
action_71 (10) = happyGoto action_75
action_71 (16) = happyGoto action_76
action_71 _ = happyFail (happyExpListPerState 71)

action_72 (19) = happyShift action_74
action_72 (14) = happyGoto action_73
action_72 _ = happyFail (happyExpListPerState 72)

action_73 _ = happyReduce_33

action_74 (27) = happyShift action_97
action_74 _ = happyReduce_24

action_75 (33) = happyShift action_96
action_75 _ = happyFail (happyExpListPerState 75)

action_76 (37) = happyShift action_95
action_76 (54) = happyShift action_90
action_76 (56) = happyShift action_91
action_76 _ = happyFail (happyExpListPerState 76)

action_77 _ = happyReduce_27

action_78 _ = happyReduce_28

action_79 (18) = happyShift action_42
action_79 (52) = happyShift action_77
action_79 (53) = happyShift action_78
action_79 (55) = happyShift action_79
action_79 (10) = happyGoto action_75
action_79 (16) = happyGoto action_94
action_79 _ = happyFail (happyExpListPerState 79)

action_80 (18) = happyShift action_93
action_80 _ = happyFail (happyExpListPerState 80)

action_81 (18) = happyShift action_42
action_81 (10) = happyGoto action_92
action_81 _ = happyFail (happyExpListPerState 81)

action_82 _ = happyReduce_20

action_83 (54) = happyShift action_90
action_83 (56) = happyShift action_91
action_83 _ = happyReduce_37

action_84 (41) = happyShift action_89
action_84 _ = happyFail (happyExpListPerState 84)

action_85 (18) = happyShift action_42
action_85 (10) = happyGoto action_88
action_85 _ = happyFail (happyExpListPerState 85)

action_86 _ = happyReduce_1

action_87 _ = happyReduce_18

action_88 (20) = happyShift action_57
action_88 (13) = happyGoto action_106
action_88 _ = happyReduce_22

action_89 (24) = happyShift action_105
action_89 _ = happyFail (happyExpListPerState 89)

action_90 (18) = happyShift action_42
action_90 (52) = happyShift action_77
action_90 (53) = happyShift action_78
action_90 (55) = happyShift action_79
action_90 (10) = happyGoto action_75
action_90 (16) = happyGoto action_104
action_90 _ = happyFail (happyExpListPerState 90)

action_91 (18) = happyShift action_42
action_91 (52) = happyShift action_77
action_91 (53) = happyShift action_78
action_91 (55) = happyShift action_79
action_91 (10) = happyGoto action_75
action_91 (16) = happyGoto action_103
action_91 _ = happyFail (happyExpListPerState 91)

action_92 (29) = happyShift action_102
action_92 _ = happyFail (happyExpListPerState 92)

action_93 (28) = happyShift action_101
action_93 _ = happyFail (happyExpListPerState 93)

action_94 (54) = happyShift action_90
action_94 (56) = happyShift action_91
action_94 _ = happyReduce_31

action_95 (20) = happyShift action_57
action_95 (13) = happyGoto action_100
action_95 _ = happyReduce_22

action_96 (18) = happyShift action_42
action_96 (10) = happyGoto action_99
action_96 _ = happyFail (happyExpListPerState 96)

action_97 (19) = happyShift action_74
action_97 (14) = happyGoto action_98
action_97 _ = happyFail (happyExpListPerState 97)

action_98 _ = happyReduce_23

action_99 _ = happyReduce_32

action_100 (38) = happyShift action_110
action_100 _ = happyFail (happyExpListPerState 100)

action_101 (18) = happyShift action_42
action_101 (10) = happyGoto action_109
action_101 _ = happyFail (happyExpListPerState 101)

action_102 (32) = happyShift action_108
action_102 _ = happyFail (happyExpListPerState 102)

action_103 (54) = happyShift action_90
action_103 (56) = happyShift action_91
action_103 _ = happyReduce_30

action_104 (54) = happyShift action_90
action_104 (56) = happyShift action_91
action_104 _ = happyReduce_29

action_105 (18) = happyShift action_26
action_105 (7) = happyGoto action_107
action_105 _ = happyFail (happyExpListPerState 105)

action_106 _ = happyReduce_21

action_107 (25) = happyShift action_114
action_107 _ = happyFail (happyExpListPerState 107)

action_108 (18) = happyShift action_42
action_108 (10) = happyGoto action_113
action_108 _ = happyFail (happyExpListPerState 108)

action_109 (29) = happyShift action_112
action_109 _ = happyFail (happyExpListPerState 109)

action_110 (20) = happyShift action_57
action_110 (13) = happyGoto action_111
action_110 _ = happyReduce_22

action_111 (39) = happyShift action_115
action_111 _ = happyFail (happyExpListPerState 111)

action_112 _ = happyReduce_34

action_113 _ = happyReduce_35

action_114 _ = happyReduce_36

action_115 _ = happyReduce_38

happyReduce_1 = happyReduce 24 4 happyReduction_1
happyReduction_1 ((HappyTerminal (TINT _ happy_var_24)) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn13  happy_var_21) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn12  happy_var_18) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn11  happy_var_15) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn8  happy_var_12) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn6  happy_var_9) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn5  happy_var_6) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TIDENT _ happy_var_3)) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 ((happy_var_3,happy_var_6,happy_var_9,happy_var_12,happy_var_15,happy_var_18,happy_var_21,happy_var_24)
	) `HappyStk` happyRest

happyReduce_2 = happyReduce 5 5 happyReduction_2
happyReduction_2 ((HappyAbsSyn5  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TINT _ happy_var_3)) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TCONST _ happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 ((happy_var_1, happy_var_3):happy_var_5
	) `HappyStk` happyRest

happyReduce_3 = happySpecReduce_3  5 happyReduction_3
happyReduction_3 (HappyTerminal (TINT _ happy_var_3))
	_
	(HappyTerminal (TCONST _ happy_var_1))
	 =  HappyAbsSyn5
		 ([(happy_var_1, happy_var_3)]
	)
happyReduction_3 _ _ _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_2  6 happyReduction_4
happyReduction_4 (HappyAbsSyn6  happy_var_2)
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn6
		 ((Public happy_var_1):happy_var_2
	)
happyReduction_4 _ _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_3  6 happyReduction_5
happyReduction_5 (HappyAbsSyn6  happy_var_3)
	(HappyAbsSyn5  happy_var_2)
	_
	 =  HappyAbsSyn6
		 ((Private happy_var_2):happy_var_3
	)
happyReduction_5 _ _ _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_0  6 happyReduction_6
happyReduction_6  =  HappyAbsSyn6
		 ([]
	)

happyReduce_7 = happySpecReduce_3  7 happyReduction_7
happyReduction_7 (HappyAbsSyn7  happy_var_3)
	_
	(HappyTerminal (TCONST _ happy_var_1))
	 =  HappyAbsSyn7
		 (happy_var_1:happy_var_3
	)
happyReduction_7 _ _ _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_1  7 happyReduction_8
happyReduction_8 (HappyTerminal (TCONST _ happy_var_1))
	 =  HappyAbsSyn7
		 ([happy_var_1]
	)
happyReduction_8 _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_2  8 happyReduction_9
happyReduction_9 (HappyAbsSyn8  happy_var_2)
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn8
		 ((Honest happy_var_1):happy_var_2
	)
happyReduction_9 _ _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_3  8 happyReduction_10
happyReduction_10 (HappyAbsSyn8  happy_var_3)
	(HappyAbsSyn7  happy_var_2)
	_
	 =  HappyAbsSyn8
		 ((Dishonest happy_var_2):happy_var_3
	)
happyReduction_10 _ _ _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_0  8 happyReduction_11
happyReduction_11  =  HappyAbsSyn8
		 ([]
	)

happyReduce_12 = happySpecReduce_3  9 happyReduction_12
happyReduction_12 (HappyAbsSyn9  happy_var_3)
	_
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn9
		 (happy_var_1:happy_var_3
	)
happyReduction_12 _ _ _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_1  9 happyReduction_13
happyReduction_13 (HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn9
		 ([happy_var_1]
	)
happyReduction_13 _  = notHappyAtAll 

happyReduce_14 = happyReduce 4 10 happyReduction_14
happyReduction_14 (_ `HappyStk`
	(HappyAbsSyn9  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TCONST _ happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn10
		 (Comp happy_var_1 happy_var_3
	) `HappyStk` happyRest

happyReduce_15 = happySpecReduce_1  10 happyReduction_15
happyReduction_15 (HappyTerminal (TCONST _ happy_var_1))
	 =  HappyAbsSyn10
		 (Atom happy_var_1
	)
happyReduction_15 _  = notHappyAtAll 

happyReduce_16 = happyReduce 4 11 happyReduction_16
happyReduction_16 ((HappyAbsSyn11  happy_var_4) `HappyStk`
	(HappyAbsSyn9  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TIDENT _ happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn11
		 ((happy_var_1,happy_var_3):happy_var_4
	) `HappyStk` happyRest

happyReduce_17 = happySpecReduce_3  11 happyReduction_17
happyReduction_17 (HappyAbsSyn9  happy_var_3)
	_
	(HappyTerminal (TIDENT _ happy_var_1))
	 =  HappyAbsSyn11
		 ([(happy_var_1,happy_var_3)]
	)
happyReduction_17 _ _ _  = notHappyAtAll 

happyReduce_18 = happyReduce 7 12 happyReduction_18
happyReduction_18 ((HappyAbsSyn12  happy_var_7) `HappyStk`
	(HappyAbsSyn10  happy_var_6) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn10  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TCONST _ happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn12
		 ((happy_var_1, happy_var_3, happy_var_6):happy_var_7
	) `HappyStk` happyRest

happyReduce_19 = happyReduce 6 12 happyReduction_19
happyReduction_19 ((HappyAbsSyn10  happy_var_6) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn10  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TCONST _ happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn12
		 ([(happy_var_1, happy_var_3, happy_var_6)]
	) `HappyStk` happyRest

happyReduce_20 = happyReduce 4 13 happyReduction_20
happyReduction_20 ((HappyAbsSyn13  happy_var_4) `HappyStk`
	(HappyAbsSyn17  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TIDENT _ happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn13
		 ((Local happy_var_1 happy_var_3 happy_var_4)
	) `HappyStk` happyRest

happyReduce_21 = happyReduce 6 13 happyReduction_21
happyReduction_21 ((HappyAbsSyn13  happy_var_6) `HappyStk`
	(HappyAbsSyn10  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TIDENT _ happy_var_3)) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TIDENT _ happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn13
		 ((Comm happy_var_1 happy_var_3 happy_var_5 happy_var_6)
	) `HappyStk` happyRest

happyReduce_22 = happySpecReduce_0  13 happyReduction_22
happyReduction_22  =  HappyAbsSyn13
		 (End
	)

happyReduce_23 = happySpecReduce_3  14 happyReduction_23
happyReduction_23 (HappyAbsSyn14  happy_var_3)
	_
	(HappyTerminal (TVAR _ happy_var_1))
	 =  HappyAbsSyn14
		 (happy_var_1:happy_var_3
	)
happyReduction_23 _ _ _  = notHappyAtAll 

happyReduce_24 = happySpecReduce_1  14 happyReduction_24
happyReduction_24 (HappyTerminal (TVAR _ happy_var_1))
	 =  HappyAbsSyn14
		 ([happy_var_1]
	)
happyReduction_24 _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_1  15 happyReduction_25
happyReduction_25 _
	 =  HappyAbsSyn15
		 (MStar
	)

happyReduce_26 = happySpecReduce_1  15 happyReduction_26
happyReduction_26 _
	 =  HappyAbsSyn15
		 (MDiamond
	)

happyReduce_27 = happySpecReduce_1  16 happyReduction_27
happyReduction_27 _
	 =  HappyAbsSyn16
		 (BTrue
	)

happyReduce_28 = happySpecReduce_1  16 happyReduction_28
happyReduction_28 _
	 =  HappyAbsSyn16
		 ((BNot BTrue)
	)

happyReduce_29 = happySpecReduce_3  16 happyReduction_29
happyReduction_29 (HappyAbsSyn16  happy_var_3)
	_
	(HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn16
		 ((BAnd happy_var_1 happy_var_3)
	)
happyReduction_29 _ _ _  = notHappyAtAll 

happyReduce_30 = happySpecReduce_3  16 happyReduction_30
happyReduction_30 (HappyAbsSyn16  happy_var_3)
	_
	(HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn16
		 ((BOr happy_var_1 happy_var_3)
	)
happyReduction_30 _ _ _  = notHappyAtAll 

happyReduce_31 = happySpecReduce_2  16 happyReduction_31
happyReduction_31 (HappyAbsSyn16  happy_var_2)
	_
	 =  HappyAbsSyn16
		 ((BNot happy_var_2)
	)
happyReduction_31 _ _  = notHappyAtAll 

happyReduce_32 = happySpecReduce_3  16 happyReduction_32
happyReduction_32 (HappyAbsSyn10  happy_var_3)
	_
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn16
		 ((BEq happy_var_1 happy_var_3)
	)
happyReduction_32 _ _ _  = notHappyAtAll 

happyReduce_33 = happySpecReduce_2  17 happyReduction_33
happyReduction_33 (HappyAbsSyn14  happy_var_2)
	_
	 =  HappyAbsSyn17
		 ((PNew happy_var_2)
	)
happyReduction_33 _ _  = notHappyAtAll 

happyReduce_34 = happyReduce 6 17 happyReduction_34
happyReduction_34 (_ `HappyStk`
	(HappyAbsSyn10  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TCONST _ happy_var_3)) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TVAR _ happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn17
		 ((PRead happy_var_1 happy_var_3 happy_var_5)
	) `HappyStk` happyRest

happyReduce_35 = happyReduce 6 17 happyReduction_35
happyReduction_35 ((HappyAbsSyn10  happy_var_6) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn10  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TCONST _ happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn17
		 ((PWrite happy_var_1 happy_var_3 happy_var_6)
	) `HappyStk` happyRest

happyReduce_36 = happyReduce 6 17 happyReduction_36
happyReduction_36 (_ `HappyStk`
	(HappyAbsSyn7  happy_var_5) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TVAR _ happy_var_2)) `HappyStk`
	(HappyAbsSyn15  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn17
		 ((PChoice happy_var_1 happy_var_2 happy_var_5)
	) `HappyStk` happyRest

happyReduce_37 = happySpecReduce_2  17 happyReduction_37
happyReduction_37 (HappyAbsSyn16  happy_var_2)
	(HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn17
		 ((PRelease happy_var_1 happy_var_2)
	)
happyReduction_37 _ _  = notHappyAtAll 

happyReduce_38 = happyReduce 7 17 happyReduction_38
happyReduction_38 (_ `HappyStk`
	(HappyAbsSyn13  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn13  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn16  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn17
		 ((PIf happy_var_2 happy_var_4 happy_var_6)
	) `HappyStk` happyRest

happyNewToken action sts stk [] =
	action 57 57 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TCONST _ happy_dollar_dollar -> cont 18;
	TVAR _ happy_dollar_dollar -> cont 19;
	TIDENT _ happy_dollar_dollar -> cont 20;
	TINT _ happy_dollar_dollar -> cont 21;
	TOPENP _ -> cont 22;
	TCLOSEP _ -> cont 23;
	TOPENB _ -> cont 24;
	TCLOSEB _ -> cont 25;
	TCOLON _ -> cont 26;
	TCOMMA _ -> cont 27;
	TOPENSQB _ -> cont 28;
	TCLOSESQB _ -> cont 29;
	TDIAMOND _ -> cont 30;
	TSTAR _ -> cont 31;
	TASSIGN _ -> cont 32;
	TEQUAL _ -> cont 33;
	TSLASH _ -> cont 34;
	TCHANNEL _ -> cont 35;
	TIF _ -> cont 36;
	TTHEN _ -> cont 37;
	TELSE _ -> cont 38;
	TEND _ -> cont 39;
	TNEW _ -> cont 40;
	TIN _ -> cont 41;
	TDISHONEST _ -> cont 42;
	TPRIVATE _ -> cont 43;
	TPROTOCOL _ -> cont 44;
	TSIGMA0 _ -> cont 45;
	TSIGMA _ -> cont 46;
	TCELLS _ -> cont 47;
	TAGENTS _ -> cont 48;
	TKNOWLEDGE _ -> cont 49;
	TACIONS _ -> cont 50;
	TBOUND _ -> cont 51;
	TTRUE _ -> cont 52;
	TFALSE _ -> cont 53;
	TAND _ -> cont 54;
	TNOT _ -> cont 55;
	TOR _ -> cont 56;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 57 tk tks = happyError' (tks, explist)
happyError_ explist _ tk tks = happyError' ((tk:tks), explist)

newtype HappyIdentity a = HappyIdentity a
happyIdentity = HappyIdentity
happyRunIdentity (HappyIdentity a) = a

instance Prelude.Functor HappyIdentity where
    fmap f (HappyIdentity a) = HappyIdentity (f a)

instance Applicative HappyIdentity where
    pure  = HappyIdentity
    (<*>) = ap
instance Prelude.Monad HappyIdentity where
    return = pure
    (HappyIdentity p) >>= q = q p

happyThen :: () => HappyIdentity a -> (a -> HappyIdentity b) -> HappyIdentity b
happyThen = (Prelude.>>=)
happyReturn :: () => a -> HappyIdentity a
happyReturn = (Prelude.return)
happyThen1 m k tks = (Prelude.>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> HappyIdentity a
happyReturn1 = \a tks -> (Prelude.return) a
happyError' :: () => ([(Token)], [Prelude.String]) -> HappyIdentity a
happyError' = HappyIdentity Prelude.. (\(tokens, _) -> parseError tokens)
parseAnnB tks = happyRunIdentity happySomeParser where
 happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


parseError :: [Token] -> a
parseError _ = error "Parse error"
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- $Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp $










































data Happy_IntList = HappyCons Prelude.Int Happy_IntList








































infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is ERROR_TOK, it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
         (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action









































indexShortOffAddr arr off = arr Happy_Data_Array.! off


{-# INLINE happyLt #-}
happyLt x y = (x Prelude.< y)






readArrayBit arr bit =
    Bits.testBit (indexShortOffAddr arr (bit `Prelude.div` 16)) (bit `Prelude.mod` 16)






-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Prelude.Int ->                    -- token number
         Prelude.Int ->                    -- token number (yes, again)
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
     = happyFail [] (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k Prelude.- ((1) :: Prelude.Int)) sts of
         sts1@(((st1@(HappyState (action))):(_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
         let drop_stk = happyDropStk k stk





             _ = nt :: Prelude.Int
             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n Prelude.- ((1) :: Prelude.Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n Prelude.- ((1)::Prelude.Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction









happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery (ERROR_TOK is the error token)

-- parse error if we are in recovery and we fail again
happyFail explist (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--      trace "failing" $ 
        happyError_ explist i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  ERROR_TOK tk old_st CONS(HAPPYSTATE(action),sts) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        DO_ACTION(action,ERROR_TOK,tk,sts,(saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail explist i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
        action (1) (1) tk (HappyState (action)) sts ((HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = Prelude.error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `Prelude.seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.









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
