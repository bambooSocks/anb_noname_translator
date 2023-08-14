{-# OPTIONS_GHC -w #-}
module AnnB_Parser where
import AnnB_Lexer
import Types
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
	| HappyAbsSyn7 ([CellDef])
	| HappyAbsSyn9 (Def)
	| HappyAbsSyn11 ([String])
	| HappyAbsSyn12 ([AgentDef])
	| HappyAbsSyn13 ([Msg])
	| HappyAbsSyn14 (Msg)
	| HappyAbsSyn15 ([Knowledge])
	| HappyAbsSyn16 (String)
	| HappyAbsSyn18 (Action)
	| HappyAbsSyn19 ([Var])
	| HappyAbsSyn20 (Mode)
	| HappyAbsSyn21 (Formula Msg)
	| HappyAbsSyn22 (PProcess)

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
 action_115,
 action_116,
 action_117,
 action_118,
 action_119,
 action_120,
 action_121,
 action_122 :: () => Prelude.Int -> ({-HappyReduction (HappyIdentity) = -}
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
 happyReduce_38,
 happyReduce_39,
 happyReduce_40,
 happyReduce_41,
 happyReduce_42,
 happyReduce_43,
 happyReduce_44,
 happyReduce_45,
 happyReduce_46,
 happyReduce_47,
 happyReduce_48,
 happyReduce_49 :: () => ({-HappyReduction (HappyIdentity) = -}
	   Prelude.Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (HappyIdentity) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (HappyIdentity) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (HappyIdentity) HappyAbsSyn)

happyExpList :: Happy_Data_Array.Array Prelude.Int Prelude.Int
happyExpList = Happy_Data_Array.listArray (0,208) ([0,0,32768,0,0,0,4096,0,0,128,0,0,0,0,0,0,3072,0,0,0,0,0,2,0,0,0,0,0,0,0,0,0,0,512,0,0,4,0,0,256,0,0,0,0,0,4,0,512,0,0,32768,0,128,0,0,8,0,0,0,0,0,0,16384,0,0,0,0,4,0,0,4,0,0,8192,0,0,0,1024,0,2,0,128,32768,0,0,0,0,0,0,2,0,0,16384,0,64,0,0,0,0,0,256,32768,0,0,0,0,8,0,1024,0,0,32768,0,0,0,0,0,0,0,0,0,0,0,64,8192,0,0,8,0,0,0,128,0,0,0,0,0,0,0,0,0,0,384,0,0,0,0,0,0,0,0,0,0,0,0,1024,0,0,4,0,0,768,0,0,0,0,0,16,0,512,0,0,32768,0,0,0,0,8,0,0,1536,0,0,0,16384,0,0,0,64,0,0,0,0,0,0,24576,0,0,0,3072,0,0,0,0,0,0,0,48,0,0,0,0,0,0,0,128,0,0,6144,0,0,0,32768,256,0,0,0,0,32,0,0,0,0,0,8,0,0,0,0,0,0,0,1,0,0,192,2182,0,0,24,0,0,0,1024,0,0,0,1024,0,0,0,2,0,0,384,0,2816,0,48,0,0,0,1024,0,0,0,2048,0,0,0,0,0,0,0,0,0,0,96,0,704,0,8,0,0,0,2,0,0,0,0,0,0,0,0,0,0,16384,0,0,0,0,2,0,0,0,4,40,0,0,0,0,0,0,0,0,384,0,2816,0,16,0,0,0,6,0,0,0,0,0,0,0,0,16384,1,0,16384,0,0,96,0,0,0,12,0,0,32768,0,0,0,12288,0,0,0,16384,0,0,0,192,0,1408,0,24,0,176,0,1024,0,0,0,64,0,0,0,0,0,0,384,0,0,0,48,0,0,0,4,0,0,0,0,0,0,0,0,0,0,0,2048,0,0,96,0,0,0,32768,0,0,0,0,0,0,0,0,0,0,512,0,0,0,0,0,0,0,0,0,0,0,64,0,0,24576,0,0,0,0,16,0,0,384,0,0,0,0,256,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_parseAnnB","annbspec","sigma0","sigma","cells","defs","def","sigmaDefs","consts","agents","messages","message","knowledges","ident","cellDefs","actions","vars","mode","formula","process","const","var","int","\"(\"","\")\"","\"{\"","\"}\"","\":\"","\",\"","\"[\"","\"]\"","\"<>\"","\"*\"","\":=\"","\"=\"","\"/\"","\"->\"","\"if\"","\"then\"","\"else\"","\"end\"","\"new\"","\"in\"","\"dishonest\"","\"private\"","\"Protocol\"","\"Sigma0\"","\"Sigma\"","\"Cells\"","\"Agents\"","\"Knowledge\"","\"Actions\"","\"Bound\"","\"true\"","\"false\"","\"and\"","\"not\"","\"or\"","%eof"]
        bit_start = st Prelude.* 61
        bit_end = (st Prelude.+ 1) Prelude.* 61
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..60]
        token_strs_expected = Prelude.concatMap f bits_indexed
        f (Prelude.False, _) = []
        f (Prelude.True, nr) = [token_strs Prelude.!! nr]

action_0 (48) = happyShift action_2
action_0 (4) = happyGoto action_3
action_0 _ = happyFail (happyExpListPerState 0)

action_1 (48) = happyShift action_2
action_1 _ = happyFail (happyExpListPerState 1)

action_2 (30) = happyShift action_4
action_2 _ = happyFail (happyExpListPerState 2)

action_3 (61) = happyAccept
action_3 _ = happyFail (happyExpListPerState 3)

action_4 (23) = happyShift action_6
action_4 (24) = happyShift action_7
action_4 (16) = happyGoto action_5
action_4 _ = happyFail (happyExpListPerState 4)

action_5 (49) = happyShift action_9
action_5 (5) = happyGoto action_8
action_5 _ = happyReduce_3

action_6 _ = happyReduce_27

action_7 _ = happyReduce_28

action_8 (50) = happyShift action_12
action_8 (6) = happyGoto action_11
action_8 _ = happyReduce_5

action_9 (30) = happyShift action_10
action_9 _ = happyFail (happyExpListPerState 9)

action_10 (23) = happyShift action_17
action_10 (8) = happyGoto action_15
action_10 (9) = happyGoto action_16
action_10 _ = happyFail (happyExpListPerState 10)

action_11 (52) = happyShift action_14
action_11 _ = happyFail (happyExpListPerState 11)

action_12 (30) = happyShift action_13
action_12 _ = happyFail (happyExpListPerState 12)

action_13 (23) = happyShift action_17
action_13 (47) = happyShift action_23
action_13 (8) = happyGoto action_21
action_13 (9) = happyGoto action_16
action_13 (10) = happyGoto action_22
action_13 _ = happyReduce_14

action_14 (30) = happyShift action_20
action_14 _ = happyFail (happyExpListPerState 14)

action_15 _ = happyReduce_2

action_16 (31) = happyShift action_19
action_16 _ = happyReduce_9

action_17 (38) = happyShift action_18
action_17 _ = happyReduce_11

action_18 (25) = happyShift action_31
action_18 _ = happyFail (happyExpListPerState 18)

action_19 (23) = happyShift action_17
action_19 (8) = happyGoto action_30
action_19 (9) = happyGoto action_16
action_19 _ = happyFail (happyExpListPerState 19)

action_20 (23) = happyShift action_28
action_20 (46) = happyShift action_29
action_20 (11) = happyGoto action_26
action_20 (12) = happyGoto action_27
action_20 _ = happyReduce_19

action_21 (23) = happyShift action_17
action_21 (47) = happyShift action_23
action_21 (8) = happyGoto action_21
action_21 (9) = happyGoto action_16
action_21 (10) = happyGoto action_25
action_21 _ = happyReduce_14

action_22 _ = happyReduce_4

action_23 (23) = happyShift action_17
action_23 (8) = happyGoto action_24
action_23 (9) = happyGoto action_16
action_23 _ = happyFail (happyExpListPerState 23)

action_24 (23) = happyShift action_17
action_24 (47) = happyShift action_23
action_24 (8) = happyGoto action_21
action_24 (9) = happyGoto action_16
action_24 (10) = happyGoto action_36
action_24 _ = happyReduce_14

action_25 _ = happyReduce_12

action_26 (23) = happyShift action_28
action_26 (46) = happyShift action_29
action_26 (11) = happyGoto action_26
action_26 (12) = happyGoto action_35
action_26 _ = happyReduce_19

action_27 (53) = happyShift action_34
action_27 _ = happyFail (happyExpListPerState 27)

action_28 (31) = happyShift action_33
action_28 _ = happyReduce_16

action_29 (23) = happyShift action_28
action_29 (11) = happyGoto action_32
action_29 _ = happyFail (happyExpListPerState 29)

action_30 _ = happyReduce_8

action_31 _ = happyReduce_10

action_32 (23) = happyShift action_28
action_32 (46) = happyShift action_29
action_32 (11) = happyGoto action_26
action_32 (12) = happyGoto action_39
action_32 _ = happyReduce_19

action_33 (23) = happyShift action_28
action_33 (11) = happyGoto action_38
action_33 _ = happyFail (happyExpListPerState 33)

action_34 (30) = happyShift action_37
action_34 _ = happyFail (happyExpListPerState 34)

action_35 _ = happyReduce_17

action_36 _ = happyReduce_13

action_37 (23) = happyShift action_6
action_37 (24) = happyShift action_7
action_37 (15) = happyGoto action_40
action_37 (16) = happyGoto action_41
action_37 _ = happyFail (happyExpListPerState 37)

action_38 _ = happyReduce_15

action_39 _ = happyReduce_18

action_40 (51) = happyShift action_44
action_40 (7) = happyGoto action_43
action_40 _ = happyReduce_7

action_41 (30) = happyShift action_42
action_41 _ = happyFail (happyExpListPerState 41)

action_42 (23) = happyShift action_49
action_42 (24) = happyShift action_50
action_42 (13) = happyGoto action_47
action_42 (14) = happyGoto action_48
action_42 _ = happyFail (happyExpListPerState 42)

action_43 (54) = happyShift action_46
action_43 _ = happyFail (happyExpListPerState 43)

action_44 (30) = happyShift action_45
action_44 _ = happyFail (happyExpListPerState 44)

action_45 (23) = happyShift action_56
action_45 (17) = happyGoto action_55
action_45 _ = happyFail (happyExpListPerState 45)

action_46 (30) = happyShift action_54
action_46 _ = happyFail (happyExpListPerState 46)

action_47 (23) = happyShift action_6
action_47 (24) = happyShift action_7
action_47 (15) = happyGoto action_53
action_47 (16) = happyGoto action_41
action_47 _ = happyReduce_26

action_48 (31) = happyShift action_52
action_48 _ = happyReduce_21

action_49 (26) = happyShift action_51
action_49 _ = happyReduce_23

action_50 _ = happyReduce_24

action_51 (23) = happyShift action_49
action_51 (24) = happyShift action_50
action_51 (13) = happyGoto action_61
action_51 (14) = happyGoto action_48
action_51 _ = happyFail (happyExpListPerState 51)

action_52 (23) = happyShift action_49
action_52 (24) = happyShift action_50
action_52 (13) = happyGoto action_60
action_52 (14) = happyGoto action_48
action_52 _ = happyFail (happyExpListPerState 52)

action_53 _ = happyReduce_25

action_54 (23) = happyShift action_6
action_54 (24) = happyShift action_7
action_54 (16) = happyGoto action_58
action_54 (18) = happyGoto action_59
action_54 _ = happyReduce_33

action_55 _ = happyReduce_6

action_56 (32) = happyShift action_57
action_56 _ = happyFail (happyExpListPerState 56)

action_57 (23) = happyShift action_49
action_57 (24) = happyShift action_50
action_57 (14) = happyGoto action_66
action_57 _ = happyFail (happyExpListPerState 57)

action_58 (30) = happyShift action_64
action_58 (39) = happyShift action_65
action_58 _ = happyFail (happyExpListPerState 58)

action_59 (55) = happyShift action_63
action_59 _ = happyFail (happyExpListPerState 59)

action_60 _ = happyReduce_20

action_61 (27) = happyShift action_62
action_61 _ = happyFail (happyExpListPerState 61)

action_62 _ = happyReduce_22

action_63 (30) = happyShift action_77
action_63 _ = happyFail (happyExpListPerState 63)

action_64 (23) = happyShift action_71
action_64 (24) = happyShift action_72
action_64 (34) = happyShift action_73
action_64 (35) = happyShift action_74
action_64 (40) = happyShift action_75
action_64 (44) = happyShift action_76
action_64 (20) = happyGoto action_69
action_64 (22) = happyGoto action_70
action_64 _ = happyFail (happyExpListPerState 64)

action_65 (23) = happyShift action_6
action_65 (24) = happyShift action_7
action_65 (16) = happyGoto action_68
action_65 _ = happyFail (happyExpListPerState 65)

action_66 (33) = happyShift action_67
action_66 _ = happyFail (happyExpListPerState 66)

action_67 (36) = happyShift action_92
action_67 _ = happyFail (happyExpListPerState 67)

action_68 (30) = happyShift action_91
action_68 _ = happyFail (happyExpListPerState 68)

action_69 (23) = happyShift action_49
action_69 (24) = happyShift action_90
action_69 (56) = happyShift action_83
action_69 (57) = happyShift action_84
action_69 (59) = happyShift action_85
action_69 (14) = happyGoto action_81
action_69 (21) = happyGoto action_89
action_69 _ = happyFail (happyExpListPerState 69)

action_70 (23) = happyShift action_6
action_70 (24) = happyShift action_7
action_70 (16) = happyGoto action_58
action_70 (18) = happyGoto action_88
action_70 _ = happyReduce_33

action_71 (32) = happyShift action_87
action_71 _ = happyFail (happyExpListPerState 71)

action_72 (36) = happyShift action_86
action_72 _ = happyFail (happyExpListPerState 72)

action_73 _ = happyReduce_37

action_74 _ = happyReduce_36

action_75 (23) = happyShift action_49
action_75 (24) = happyShift action_50
action_75 (56) = happyShift action_83
action_75 (57) = happyShift action_84
action_75 (59) = happyShift action_85
action_75 (14) = happyGoto action_81
action_75 (21) = happyGoto action_82
action_75 _ = happyFail (happyExpListPerState 75)

action_76 (24) = happyShift action_80
action_76 (19) = happyGoto action_79
action_76 _ = happyFail (happyExpListPerState 76)

action_77 (25) = happyShift action_78
action_77 _ = happyFail (happyExpListPerState 77)

action_78 _ = happyReduce_1

action_79 _ = happyReduce_44

action_80 (31) = happyShift action_103
action_80 _ = happyReduce_35

action_81 (37) = happyShift action_102
action_81 _ = happyFail (happyExpListPerState 81)

action_82 (41) = happyShift action_101
action_82 (58) = happyShift action_96
action_82 (60) = happyShift action_97
action_82 _ = happyFail (happyExpListPerState 82)

action_83 _ = happyReduce_38

action_84 _ = happyReduce_39

action_85 (23) = happyShift action_49
action_85 (24) = happyShift action_50
action_85 (56) = happyShift action_83
action_85 (57) = happyShift action_84
action_85 (59) = happyShift action_85
action_85 (14) = happyGoto action_81
action_85 (21) = happyGoto action_100
action_85 _ = happyFail (happyExpListPerState 85)

action_86 (23) = happyShift action_99
action_86 _ = happyFail (happyExpListPerState 86)

action_87 (23) = happyShift action_49
action_87 (24) = happyShift action_50
action_87 (14) = happyGoto action_98
action_87 _ = happyFail (happyExpListPerState 87)

action_88 _ = happyReduce_31

action_89 (58) = happyShift action_96
action_89 (60) = happyShift action_97
action_89 _ = happyReduce_48

action_90 (45) = happyShift action_95
action_90 _ = happyReduce_24

action_91 (23) = happyShift action_49
action_91 (24) = happyShift action_50
action_91 (14) = happyGoto action_94
action_91 _ = happyFail (happyExpListPerState 91)

action_92 (23) = happyShift action_49
action_92 (24) = happyShift action_50
action_92 (14) = happyGoto action_93
action_92 _ = happyFail (happyExpListPerState 92)

action_93 (23) = happyShift action_56
action_93 (17) = happyGoto action_113
action_93 _ = happyReduce_30

action_94 (23) = happyShift action_6
action_94 (24) = happyShift action_7
action_94 (16) = happyGoto action_58
action_94 (18) = happyGoto action_112
action_94 _ = happyReduce_33

action_95 (28) = happyShift action_111
action_95 _ = happyFail (happyExpListPerState 95)

action_96 (23) = happyShift action_49
action_96 (24) = happyShift action_50
action_96 (56) = happyShift action_83
action_96 (57) = happyShift action_84
action_96 (59) = happyShift action_85
action_96 (14) = happyGoto action_81
action_96 (21) = happyGoto action_110
action_96 _ = happyFail (happyExpListPerState 96)

action_97 (23) = happyShift action_49
action_97 (24) = happyShift action_50
action_97 (56) = happyShift action_83
action_97 (57) = happyShift action_84
action_97 (59) = happyShift action_85
action_97 (14) = happyGoto action_81
action_97 (21) = happyGoto action_109
action_97 _ = happyFail (happyExpListPerState 97)

action_98 (33) = happyShift action_108
action_98 _ = happyFail (happyExpListPerState 98)

action_99 (32) = happyShift action_107
action_99 _ = happyFail (happyExpListPerState 99)

action_100 (58) = happyShift action_96
action_100 (60) = happyShift action_97
action_100 _ = happyReduce_42

action_101 (23) = happyShift action_6
action_101 (24) = happyShift action_7
action_101 (16) = happyGoto action_58
action_101 (18) = happyGoto action_106
action_101 _ = happyReduce_33

action_102 (23) = happyShift action_49
action_102 (24) = happyShift action_50
action_102 (14) = happyGoto action_105
action_102 _ = happyFail (happyExpListPerState 102)

action_103 (24) = happyShift action_80
action_103 (19) = happyGoto action_104
action_103 _ = happyFail (happyExpListPerState 103)

action_104 _ = happyReduce_34

action_105 _ = happyReduce_43

action_106 (42) = happyShift action_117
action_106 _ = happyFail (happyExpListPerState 106)

action_107 (23) = happyShift action_49
action_107 (24) = happyShift action_50
action_107 (14) = happyGoto action_116
action_107 _ = happyFail (happyExpListPerState 107)

action_108 (36) = happyShift action_115
action_108 _ = happyFail (happyExpListPerState 108)

action_109 (58) = happyShift action_96
action_109 (60) = happyShift action_97
action_109 _ = happyReduce_41

action_110 (58) = happyShift action_96
action_110 (60) = happyShift action_97
action_110 _ = happyReduce_40

action_111 (23) = happyShift action_28
action_111 (11) = happyGoto action_114
action_111 _ = happyFail (happyExpListPerState 111)

action_112 _ = happyReduce_32

action_113 _ = happyReduce_29

action_114 (29) = happyShift action_121
action_114 _ = happyFail (happyExpListPerState 114)

action_115 (23) = happyShift action_49
action_115 (24) = happyShift action_50
action_115 (14) = happyGoto action_120
action_115 _ = happyFail (happyExpListPerState 115)

action_116 (33) = happyShift action_119
action_116 _ = happyFail (happyExpListPerState 116)

action_117 (23) = happyShift action_6
action_117 (24) = happyShift action_7
action_117 (16) = happyGoto action_58
action_117 (18) = happyGoto action_118
action_117 _ = happyReduce_33

action_118 (43) = happyShift action_122
action_118 _ = happyFail (happyExpListPerState 118)

action_119 _ = happyReduce_45

action_120 _ = happyReduce_46

action_121 _ = happyReduce_47

action_122 _ = happyReduce_49

happyReduce_1 = happyReduce 18 4 happyReduction_1
happyReduction_1 ((HappyTerminal (TINT _ happy_var_18)) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn18  happy_var_15) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn7  happy_var_12) `HappyStk`
	(HappyAbsSyn15  happy_var_11) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn12  happy_var_8) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn6  happy_var_5) `HappyStk`
	(HappyAbsSyn5  happy_var_4) `HappyStk`
	(HappyAbsSyn16  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 ((happy_var_3,happy_var_4,happy_var_5,happy_var_8,happy_var_11,happy_var_12,happy_var_15,happy_var_18)
	) `HappyStk` happyRest

happyReduce_2 = happySpecReduce_3  5 happyReduction_2
happyReduction_2 (HappyAbsSyn5  happy_var_3)
	_
	_
	 =  HappyAbsSyn5
		 (happy_var_3
	)
happyReduction_2 _ _ _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_0  5 happyReduction_3
happyReduction_3  =  HappyAbsSyn5
		 ([]
	)

happyReduce_4 = happySpecReduce_3  6 happyReduction_4
happyReduction_4 (HappyAbsSyn6  happy_var_3)
	_
	_
	 =  HappyAbsSyn6
		 (happy_var_3
	)
happyReduction_4 _ _ _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_0  6 happyReduction_5
happyReduction_5  =  HappyAbsSyn6
		 ([]
	)

happyReduce_6 = happySpecReduce_3  7 happyReduction_6
happyReduction_6 (HappyAbsSyn7  happy_var_3)
	_
	_
	 =  HappyAbsSyn7
		 (happy_var_3
	)
happyReduction_6 _ _ _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_0  7 happyReduction_7
happyReduction_7  =  HappyAbsSyn7
		 ([]
	)

happyReduce_8 = happySpecReduce_3  8 happyReduction_8
happyReduction_8 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn5
		 (happy_var_1:happy_var_3
	)
happyReduction_8 _ _ _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_1  8 happyReduction_9
happyReduction_9 (HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn5
		 ([happy_var_1]
	)
happyReduction_9 _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_3  9 happyReduction_10
happyReduction_10 (HappyTerminal (TINT _ happy_var_3))
	_
	(HappyTerminal (TCONST _ happy_var_1))
	 =  HappyAbsSyn9
		 ((happy_var_1,happy_var_3)
	)
happyReduction_10 _ _ _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_1  9 happyReduction_11
happyReduction_11 (HappyTerminal (TCONST _ happy_var_1))
	 =  HappyAbsSyn9
		 ((happy_var_1, 0)
	)
happyReduction_11 _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_2  10 happyReduction_12
happyReduction_12 (HappyAbsSyn6  happy_var_2)
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn6
		 ((Public happy_var_1):happy_var_2
	)
happyReduction_12 _ _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_3  10 happyReduction_13
happyReduction_13 (HappyAbsSyn6  happy_var_3)
	(HappyAbsSyn5  happy_var_2)
	_
	 =  HappyAbsSyn6
		 ((Private happy_var_2):happy_var_3
	)
happyReduction_13 _ _ _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_0  10 happyReduction_14
happyReduction_14  =  HappyAbsSyn6
		 ([]
	)

happyReduce_15 = happySpecReduce_3  11 happyReduction_15
happyReduction_15 (HappyAbsSyn11  happy_var_3)
	_
	(HappyTerminal (TCONST _ happy_var_1))
	 =  HappyAbsSyn11
		 (happy_var_1:happy_var_3
	)
happyReduction_15 _ _ _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_1  11 happyReduction_16
happyReduction_16 (HappyTerminal (TCONST _ happy_var_1))
	 =  HappyAbsSyn11
		 ([happy_var_1]
	)
happyReduction_16 _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_2  12 happyReduction_17
happyReduction_17 (HappyAbsSyn12  happy_var_2)
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn12
		 ((Honest happy_var_1):happy_var_2
	)
happyReduction_17 _ _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_3  12 happyReduction_18
happyReduction_18 (HappyAbsSyn12  happy_var_3)
	(HappyAbsSyn11  happy_var_2)
	_
	 =  HappyAbsSyn12
		 ((Dishonest happy_var_2):happy_var_3
	)
happyReduction_18 _ _ _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_0  12 happyReduction_19
happyReduction_19  =  HappyAbsSyn12
		 ([]
	)

happyReduce_20 = happySpecReduce_3  13 happyReduction_20
happyReduction_20 (HappyAbsSyn13  happy_var_3)
	_
	(HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn13
		 (happy_var_1:happy_var_3
	)
happyReduction_20 _ _ _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_1  13 happyReduction_21
happyReduction_21 (HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn13
		 ([happy_var_1]
	)
happyReduction_21 _  = notHappyAtAll 

happyReduce_22 = happyReduce 4 14 happyReduction_22
happyReduction_22 (_ `HappyStk`
	(HappyAbsSyn13  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TCONST _ happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn14
		 (Comp happy_var_1 happy_var_3
	) `HappyStk` happyRest

happyReduce_23 = happySpecReduce_1  14 happyReduction_23
happyReduction_23 (HappyTerminal (TCONST _ happy_var_1))
	 =  HappyAbsSyn14
		 (Atom happy_var_1
	)
happyReduction_23 _  = notHappyAtAll 

happyReduce_24 = happySpecReduce_1  14 happyReduction_24
happyReduction_24 (HappyTerminal (TVAR _ happy_var_1))
	 =  HappyAbsSyn14
		 (Atom happy_var_1
	)
happyReduction_24 _  = notHappyAtAll 

happyReduce_25 = happyReduce 4 15 happyReduction_25
happyReduction_25 ((HappyAbsSyn15  happy_var_4) `HappyStk`
	(HappyAbsSyn13  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn16  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn15
		 ((happy_var_1,happy_var_3):happy_var_4
	) `HappyStk` happyRest

happyReduce_26 = happySpecReduce_3  15 happyReduction_26
happyReduction_26 (HappyAbsSyn13  happy_var_3)
	_
	(HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn15
		 ([(happy_var_1,happy_var_3)]
	)
happyReduction_26 _ _ _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_1  16 happyReduction_27
happyReduction_27 (HappyTerminal (TCONST _ happy_var_1))
	 =  HappyAbsSyn16
		 (happy_var_1
	)
happyReduction_27 _  = notHappyAtAll 

happyReduce_28 = happySpecReduce_1  16 happyReduction_28
happyReduction_28 (HappyTerminal (TVAR _ happy_var_1))
	 =  HappyAbsSyn16
		 (happy_var_1
	)
happyReduction_28 _  = notHappyAtAll 

happyReduce_29 = happyReduce 7 17 happyReduction_29
happyReduction_29 ((HappyAbsSyn7  happy_var_7) `HappyStk`
	(HappyAbsSyn14  happy_var_6) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn14  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TCONST _ happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 ((happy_var_1, happy_var_3, happy_var_6):happy_var_7
	) `HappyStk` happyRest

happyReduce_30 = happyReduce 6 17 happyReduction_30
happyReduction_30 ((HappyAbsSyn14  happy_var_6) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn14  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TCONST _ happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 ([(happy_var_1, happy_var_3, happy_var_6)]
	) `HappyStk` happyRest

happyReduce_31 = happyReduce 4 18 happyReduction_31
happyReduction_31 ((HappyAbsSyn18  happy_var_4) `HappyStk`
	(HappyAbsSyn22  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn16  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn18
		 ((Local happy_var_1 happy_var_3 happy_var_4)
	) `HappyStk` happyRest

happyReduce_32 = happyReduce 6 18 happyReduction_32
happyReduction_32 ((HappyAbsSyn18  happy_var_6) `HappyStk`
	(HappyAbsSyn14  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn16  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn16  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn18
		 ((Comm happy_var_1 happy_var_3 happy_var_5 happy_var_6)
	) `HappyStk` happyRest

happyReduce_33 = happySpecReduce_0  18 happyReduction_33
happyReduction_33  =  HappyAbsSyn18
		 (End
	)

happyReduce_34 = happySpecReduce_3  19 happyReduction_34
happyReduction_34 (HappyAbsSyn19  happy_var_3)
	_
	(HappyTerminal (TVAR _ happy_var_1))
	 =  HappyAbsSyn19
		 (happy_var_1:happy_var_3
	)
happyReduction_34 _ _ _  = notHappyAtAll 

happyReduce_35 = happySpecReduce_1  19 happyReduction_35
happyReduction_35 (HappyTerminal (TVAR _ happy_var_1))
	 =  HappyAbsSyn19
		 ([happy_var_1]
	)
happyReduction_35 _  = notHappyAtAll 

happyReduce_36 = happySpecReduce_1  20 happyReduction_36
happyReduction_36 _
	 =  HappyAbsSyn20
		 (MStar
	)

happyReduce_37 = happySpecReduce_1  20 happyReduction_37
happyReduction_37 _
	 =  HappyAbsSyn20
		 (MDiamond
	)

happyReduce_38 = happySpecReduce_1  21 happyReduction_38
happyReduction_38 _
	 =  HappyAbsSyn21
		 (BTrue
	)

happyReduce_39 = happySpecReduce_1  21 happyReduction_39
happyReduction_39 _
	 =  HappyAbsSyn21
		 ((BNot BTrue)
	)

happyReduce_40 = happySpecReduce_3  21 happyReduction_40
happyReduction_40 (HappyAbsSyn21  happy_var_3)
	_
	(HappyAbsSyn21  happy_var_1)
	 =  HappyAbsSyn21
		 ((BAnd happy_var_1 happy_var_3)
	)
happyReduction_40 _ _ _  = notHappyAtAll 

happyReduce_41 = happySpecReduce_3  21 happyReduction_41
happyReduction_41 (HappyAbsSyn21  happy_var_3)
	_
	(HappyAbsSyn21  happy_var_1)
	 =  HappyAbsSyn21
		 ((BOr happy_var_1 happy_var_3)
	)
happyReduction_41 _ _ _  = notHappyAtAll 

happyReduce_42 = happySpecReduce_2  21 happyReduction_42
happyReduction_42 (HappyAbsSyn21  happy_var_2)
	_
	 =  HappyAbsSyn21
		 ((BNot happy_var_2)
	)
happyReduction_42 _ _  = notHappyAtAll 

happyReduce_43 = happySpecReduce_3  21 happyReduction_43
happyReduction_43 (HappyAbsSyn14  happy_var_3)
	_
	(HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn21
		 ((BEq happy_var_1 happy_var_3)
	)
happyReduction_43 _ _ _  = notHappyAtAll 

happyReduce_44 = happySpecReduce_2  22 happyReduction_44
happyReduction_44 (HappyAbsSyn19  happy_var_2)
	_
	 =  HappyAbsSyn22
		 ((PNew happy_var_2)
	)
happyReduction_44 _ _  = notHappyAtAll 

happyReduce_45 = happyReduce 6 22 happyReduction_45
happyReduction_45 (_ `HappyStk`
	(HappyAbsSyn14  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TCONST _ happy_var_3)) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TVAR _ happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn22
		 ((PRead happy_var_1 happy_var_3 happy_var_5)
	) `HappyStk` happyRest

happyReduce_46 = happyReduce 6 22 happyReduction_46
happyReduction_46 ((HappyAbsSyn14  happy_var_6) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn14  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TCONST _ happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn22
		 ((PWrite happy_var_1 happy_var_3 happy_var_6)
	) `HappyStk` happyRest

happyReduce_47 = happyReduce 6 22 happyReduction_47
happyReduction_47 (_ `HappyStk`
	(HappyAbsSyn11  happy_var_5) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TVAR _ happy_var_2)) `HappyStk`
	(HappyAbsSyn20  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn22
		 ((PChoice happy_var_1 happy_var_2 happy_var_5)
	) `HappyStk` happyRest

happyReduce_48 = happySpecReduce_2  22 happyReduction_48
happyReduction_48 (HappyAbsSyn21  happy_var_2)
	(HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn22
		 ((PRelease happy_var_1 happy_var_2)
	)
happyReduction_48 _ _  = notHappyAtAll 

happyReduce_49 = happyReduce 7 22 happyReduction_49
happyReduction_49 (_ `HappyStk`
	(HappyAbsSyn18  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn18  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn21  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn22
		 ((PIf happy_var_2 happy_var_4 happy_var_6)
	) `HappyStk` happyRest

happyNewToken action sts stk [] =
	action 61 61 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TCONST _ happy_dollar_dollar -> cont 23;
	TVAR _ happy_dollar_dollar -> cont 24;
	TINT _ happy_dollar_dollar -> cont 25;
	TOPENP _ -> cont 26;
	TCLOSEP _ -> cont 27;
	TOPENB _ -> cont 28;
	TCLOSEB _ -> cont 29;
	TCOLON _ -> cont 30;
	TCOMMA _ -> cont 31;
	TOPENSQB _ -> cont 32;
	TCLOSESQB _ -> cont 33;
	TDIAMOND _ -> cont 34;
	TSTAR _ -> cont 35;
	TASSIGN _ -> cont 36;
	TEQUAL _ -> cont 37;
	TSLASH _ -> cont 38;
	TCHANNEL _ -> cont 39;
	TIF _ -> cont 40;
	TTHEN _ -> cont 41;
	TELSE _ -> cont 42;
	TEND _ -> cont 43;
	TNEW _ -> cont 44;
	TIN _ -> cont 45;
	TDISHONEST _ -> cont 46;
	TPRIVATE _ -> cont 47;
	TPROTOCOL _ -> cont 48;
	TSIGMA0 _ -> cont 49;
	TSIGMA _ -> cont 50;
	TCELLS _ -> cont 51;
	TAGENTS _ -> cont 52;
	TKNOWLEDGE _ -> cont 53;
	TACIONS _ -> cont 54;
	TBOUND _ -> cont 55;
	TTRUE _ -> cont 56;
	TFALSE _ -> cont 57;
	TAND _ -> cont 58;
	TNOT _ -> cont 59;
	TOR _ -> cont 60;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 61 tk tks = happyError' (tks, explist)
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
parseError tks = error ("AnnB Parse error at " ++ lcn ++ "\n" )
	where
	lcn = case tks of
		  [] -> "end of file"
		  tk:_ -> "line " ++ show l ++ ", column " ++ show c ++ " - Token: " ++ show tk
			where
			AlexPn _ l c = tokenPosn tk
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
