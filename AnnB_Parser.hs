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
	| HappyAbsSyn13 ([RoleDef])
	| HappyAbsSyn14 ([Msg])
	| HappyAbsSyn15 (Msg)
	| HappyAbsSyn16 ([Knowledge])
	| HappyAbsSyn17 (String)
	| HappyAbsSyn19 (Action)
	| HappyAbsSyn20 ([Var])
	| HappyAbsSyn21 (Mode)
	| HappyAbsSyn22 (Formula Msg)
	| HappyAbsSyn23 (PProcess)

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
 action_122,
 action_123,
 action_124,
 action_125,
 action_126,
 action_127,
 action_128,
 action_129 :: () => Prelude.Int -> ({-HappyReduction (HappyIdentity) = -}
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
 happyReduce_49,
 happyReduce_50,
 happyReduce_51 :: () => ({-HappyReduction (HappyIdentity) = -}
	   Prelude.Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (HappyIdentity) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (HappyIdentity) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (HappyIdentity) HappyAbsSyn)

happyExpList :: Happy_Data_Array.Array Prelude.Int Prelude.Int
happyExpList = Happy_Data_Array.listArray (0,219) ([0,0,0,1,0,0,32768,0,0,4096,0,0,0,0,0,0,0,24,0,0,0,0,4096,0,0,0,0,0,0,0,0,0,0,0,1024,0,0,32,0,0,8192,0,0,0,0,0,512,0,0,4,0,0,1024,0,4,0,0,1,0,0,0,0,0,0,32768,0,0,0,0,32,0,0,128,0,0,0,16,0,0,0,8,1024,0,0,4,1024,0,0,0,0,0,0,1,0,0,32768,0,128,0,0,0,0,0,8192,0,16,0,0,0,1024,0,0,8,0,0,1024,0,0,0,0,0,0,0,0,0,0,0,128,16384,0,0,64,0,0,0,4096,0,0,0,0,0,0,0,0,0,0,0,8,0,0,0,0,0,0,0,0,0,0,0,0,16384,0,0,32,0,0,8192,0,0,0,0,8,0,0,6144,0,0,0,2048,0,0,0,0,0,0,0,0,0,16,0,16384,0,0,0,192,0,0,0,0,0,32,0,2048,0,0,0,8,0,0,0,512,0,0,0,6,0,0,0,256,0,0,0,4,0,0,0,0,0,0,24576,0,0,0,12288,0,0,0,0,0,0,0,3072,0,0,0,0,0,0,0,0,2,0,0,384,0,0,0,8192,64,0,0,0,0,64,0,0,0,0,0,128,0,0,0,0,0,0,0,256,0,0,0,6147,34,0,32768,1,0,0,0,256,0,0,0,1024,0,0,0,8,0,0,6144,0,24576,1,3072,0,0,0,0,4,0,0,0,32,0,0,0,0,0,0,0,0,0,0,96,0,1408,0,32,0,0,0,32,0,0,0,0,0,0,0,0,0,0,0,256,0,0,0,8192,0,0,0,0,1,20,0,0,0,0,0,0,0,0,6144,0,24576,1,1024,0,0,0,1536,0,0,0,0,0,0,0,0,0,10240,0,0,4096,0,0,96,0,0,0,48,0,0,0,8,0,0,0,12,0,0,0,64,0,0,0,3,0,44,32768,1,0,22,0,256,0,0,0,64,0,0,0,0,0,0,6144,0,0,0,3072,0,0,0,1024,0,0,0,0,0,0,0,0,0,0,0,0,512,0,0,96,0,0,0,0,2,0,0,0,0,0,0,0,0,0,0,2,0,0,0,0,0,0,0,0,0,0,0,16,0,0,24576,0,0,0,0,64,0,0,6144,0,0,0,0,16384,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_parseAnnB","annbspec","sigma0","sigma","cells","defs","def","sigmaDefs","consts","agents","roles","messages","message","knowledges","ident","cellDefs","actions","vars","mode","formula","process","const","var","int","\"(\"","\")\"","\"{\"","\"}\"","\":\"","\",\"","\"[\"","\"]\"","\"<>\"","\"*\"","\":=\"","\"=\"","\"/\"","\"->\"","\"if\"","\"then\"","\"else\"","\"end\"","\"new\"","\"in\"","\"dishonest\"","\"private\"","\"Protocol\"","\"Sigma0\"","\"Sigma\"","\"Cells\"","\"Agents\"","\"Roles\"","\"Knowledge\"","\"Actions\"","\"Bound\"","\"true\"","\"false\"","\"and\"","\"not\"","\"or\"","%eof"]
        bit_start = st Prelude.* 63
        bit_end = (st Prelude.+ 1) Prelude.* 63
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..62]
        token_strs_expected = Prelude.concatMap f bits_indexed
        f (Prelude.False, _) = []
        f (Prelude.True, nr) = [token_strs Prelude.!! nr]

action_0 (49) = happyShift action_2
action_0 (4) = happyGoto action_3
action_0 _ = happyFail (happyExpListPerState 0)

action_1 (49) = happyShift action_2
action_1 _ = happyFail (happyExpListPerState 1)

action_2 (31) = happyShift action_4
action_2 _ = happyFail (happyExpListPerState 2)

action_3 (63) = happyAccept
action_3 _ = happyFail (happyExpListPerState 3)

action_4 (24) = happyShift action_6
action_4 (25) = happyShift action_7
action_4 (17) = happyGoto action_5
action_4 _ = happyFail (happyExpListPerState 4)

action_5 (50) = happyShift action_9
action_5 (5) = happyGoto action_8
action_5 _ = happyReduce_3

action_6 _ = happyReduce_29

action_7 _ = happyReduce_30

action_8 (51) = happyShift action_12
action_8 (6) = happyGoto action_11
action_8 _ = happyReduce_5

action_9 (31) = happyShift action_10
action_9 _ = happyFail (happyExpListPerState 9)

action_10 (24) = happyShift action_17
action_10 (8) = happyGoto action_15
action_10 (9) = happyGoto action_16
action_10 _ = happyFail (happyExpListPerState 10)

action_11 (53) = happyShift action_14
action_11 _ = happyFail (happyExpListPerState 11)

action_12 (31) = happyShift action_13
action_12 _ = happyFail (happyExpListPerState 12)

action_13 (24) = happyShift action_17
action_13 (48) = happyShift action_23
action_13 (8) = happyGoto action_21
action_13 (9) = happyGoto action_16
action_13 (10) = happyGoto action_22
action_13 _ = happyReduce_14

action_14 (31) = happyShift action_20
action_14 _ = happyFail (happyExpListPerState 14)

action_15 _ = happyReduce_2

action_16 (32) = happyShift action_19
action_16 _ = happyReduce_9

action_17 (39) = happyShift action_18
action_17 _ = happyReduce_11

action_18 (26) = happyShift action_31
action_18 _ = happyFail (happyExpListPerState 18)

action_19 (24) = happyShift action_17
action_19 (8) = happyGoto action_30
action_19 (9) = happyGoto action_16
action_19 _ = happyFail (happyExpListPerState 19)

action_20 (24) = happyShift action_28
action_20 (47) = happyShift action_29
action_20 (11) = happyGoto action_26
action_20 (12) = happyGoto action_27
action_20 _ = happyReduce_19

action_21 (24) = happyShift action_17
action_21 (48) = happyShift action_23
action_21 (8) = happyGoto action_21
action_21 (9) = happyGoto action_16
action_21 (10) = happyGoto action_25
action_21 _ = happyReduce_14

action_22 _ = happyReduce_4

action_23 (24) = happyShift action_17
action_23 (8) = happyGoto action_24
action_23 (9) = happyGoto action_16
action_23 _ = happyFail (happyExpListPerState 23)

action_24 (24) = happyShift action_17
action_24 (48) = happyShift action_23
action_24 (8) = happyGoto action_21
action_24 (9) = happyGoto action_16
action_24 (10) = happyGoto action_36
action_24 _ = happyReduce_14

action_25 _ = happyReduce_12

action_26 (24) = happyShift action_28
action_26 (47) = happyShift action_29
action_26 (11) = happyGoto action_26
action_26 (12) = happyGoto action_35
action_26 _ = happyReduce_19

action_27 (54) = happyShift action_34
action_27 _ = happyFail (happyExpListPerState 27)

action_28 (32) = happyShift action_33
action_28 _ = happyReduce_16

action_29 (24) = happyShift action_28
action_29 (11) = happyGoto action_32
action_29 _ = happyFail (happyExpListPerState 29)

action_30 _ = happyReduce_8

action_31 _ = happyReduce_10

action_32 (24) = happyShift action_28
action_32 (47) = happyShift action_29
action_32 (11) = happyGoto action_26
action_32 (12) = happyGoto action_39
action_32 _ = happyReduce_19

action_33 (24) = happyShift action_28
action_33 (11) = happyGoto action_38
action_33 _ = happyFail (happyExpListPerState 33)

action_34 (31) = happyShift action_37
action_34 _ = happyFail (happyExpListPerState 34)

action_35 _ = happyReduce_17

action_36 _ = happyReduce_13

action_37 (25) = happyShift action_41
action_37 (13) = happyGoto action_40
action_37 _ = happyReduce_21

action_38 _ = happyReduce_15

action_39 _ = happyReduce_18

action_40 (55) = happyShift action_43
action_40 _ = happyFail (happyExpListPerState 40)

action_41 (31) = happyShift action_42
action_41 _ = happyFail (happyExpListPerState 41)

action_42 (24) = happyShift action_28
action_42 (11) = happyGoto action_45
action_42 _ = happyFail (happyExpListPerState 42)

action_43 (31) = happyShift action_44
action_43 _ = happyFail (happyExpListPerState 43)

action_44 (24) = happyShift action_6
action_44 (25) = happyShift action_7
action_44 (16) = happyGoto action_47
action_44 (17) = happyGoto action_48
action_44 _ = happyFail (happyExpListPerState 44)

action_45 (25) = happyShift action_41
action_45 (13) = happyGoto action_46
action_45 _ = happyReduce_21

action_46 _ = happyReduce_20

action_47 (52) = happyShift action_51
action_47 (7) = happyGoto action_50
action_47 _ = happyReduce_7

action_48 (31) = happyShift action_49
action_48 _ = happyFail (happyExpListPerState 48)

action_49 (24) = happyShift action_56
action_49 (25) = happyShift action_57
action_49 (14) = happyGoto action_54
action_49 (15) = happyGoto action_55
action_49 _ = happyFail (happyExpListPerState 49)

action_50 (56) = happyShift action_53
action_50 _ = happyFail (happyExpListPerState 50)

action_51 (31) = happyShift action_52
action_51 _ = happyFail (happyExpListPerState 51)

action_52 (24) = happyShift action_63
action_52 (18) = happyGoto action_62
action_52 _ = happyFail (happyExpListPerState 52)

action_53 (31) = happyShift action_61
action_53 _ = happyFail (happyExpListPerState 53)

action_54 (24) = happyShift action_6
action_54 (25) = happyShift action_7
action_54 (16) = happyGoto action_60
action_54 (17) = happyGoto action_48
action_54 _ = happyReduce_28

action_55 (32) = happyShift action_59
action_55 _ = happyReduce_23

action_56 (27) = happyShift action_58
action_56 _ = happyReduce_25

action_57 _ = happyReduce_26

action_58 (24) = happyShift action_56
action_58 (25) = happyShift action_57
action_58 (14) = happyGoto action_68
action_58 (15) = happyGoto action_55
action_58 _ = happyFail (happyExpListPerState 58)

action_59 (24) = happyShift action_56
action_59 (25) = happyShift action_57
action_59 (14) = happyGoto action_67
action_59 (15) = happyGoto action_55
action_59 _ = happyFail (happyExpListPerState 59)

action_60 _ = happyReduce_27

action_61 (24) = happyShift action_6
action_61 (25) = happyShift action_7
action_61 (17) = happyGoto action_65
action_61 (19) = happyGoto action_66
action_61 _ = happyReduce_35

action_62 _ = happyReduce_6

action_63 (33) = happyShift action_64
action_63 _ = happyFail (happyExpListPerState 63)

action_64 (24) = happyShift action_56
action_64 (25) = happyShift action_57
action_64 (15) = happyGoto action_73
action_64 _ = happyFail (happyExpListPerState 64)

action_65 (31) = happyShift action_71
action_65 (40) = happyShift action_72
action_65 _ = happyFail (happyExpListPerState 65)

action_66 (57) = happyShift action_70
action_66 _ = happyFail (happyExpListPerState 66)

action_67 _ = happyReduce_22

action_68 (28) = happyShift action_69
action_68 _ = happyFail (happyExpListPerState 68)

action_69 _ = happyReduce_24

action_70 (31) = happyShift action_84
action_70 _ = happyFail (happyExpListPerState 70)

action_71 (24) = happyShift action_78
action_71 (25) = happyShift action_79
action_71 (35) = happyShift action_80
action_71 (36) = happyShift action_81
action_71 (41) = happyShift action_82
action_71 (45) = happyShift action_83
action_71 (21) = happyGoto action_76
action_71 (23) = happyGoto action_77
action_71 _ = happyFail (happyExpListPerState 71)

action_72 (24) = happyShift action_6
action_72 (25) = happyShift action_7
action_72 (17) = happyGoto action_75
action_72 _ = happyFail (happyExpListPerState 72)

action_73 (34) = happyShift action_74
action_73 _ = happyFail (happyExpListPerState 73)

action_74 (37) = happyShift action_99
action_74 _ = happyFail (happyExpListPerState 74)

action_75 (31) = happyShift action_98
action_75 _ = happyFail (happyExpListPerState 75)

action_76 (24) = happyShift action_56
action_76 (25) = happyShift action_97
action_76 (58) = happyShift action_90
action_76 (59) = happyShift action_91
action_76 (61) = happyShift action_92
action_76 (15) = happyGoto action_88
action_76 (22) = happyGoto action_96
action_76 _ = happyFail (happyExpListPerState 76)

action_77 (24) = happyShift action_6
action_77 (25) = happyShift action_7
action_77 (17) = happyGoto action_65
action_77 (19) = happyGoto action_95
action_77 _ = happyReduce_35

action_78 (33) = happyShift action_94
action_78 _ = happyFail (happyExpListPerState 78)

action_79 (37) = happyShift action_93
action_79 _ = happyFail (happyExpListPerState 79)

action_80 _ = happyReduce_39

action_81 _ = happyReduce_38

action_82 (24) = happyShift action_56
action_82 (25) = happyShift action_57
action_82 (58) = happyShift action_90
action_82 (59) = happyShift action_91
action_82 (61) = happyShift action_92
action_82 (15) = happyGoto action_88
action_82 (22) = happyGoto action_89
action_82 _ = happyFail (happyExpListPerState 82)

action_83 (25) = happyShift action_87
action_83 (20) = happyGoto action_86
action_83 _ = happyFail (happyExpListPerState 83)

action_84 (26) = happyShift action_85
action_84 _ = happyFail (happyExpListPerState 84)

action_85 _ = happyReduce_1

action_86 _ = happyReduce_46

action_87 (32) = happyShift action_110
action_87 _ = happyReduce_37

action_88 (38) = happyShift action_109
action_88 _ = happyFail (happyExpListPerState 88)

action_89 (42) = happyShift action_108
action_89 (60) = happyShift action_103
action_89 (62) = happyShift action_104
action_89 _ = happyFail (happyExpListPerState 89)

action_90 _ = happyReduce_40

action_91 _ = happyReduce_41

action_92 (24) = happyShift action_56
action_92 (25) = happyShift action_57
action_92 (58) = happyShift action_90
action_92 (59) = happyShift action_91
action_92 (61) = happyShift action_92
action_92 (15) = happyGoto action_88
action_92 (22) = happyGoto action_107
action_92 _ = happyFail (happyExpListPerState 92)

action_93 (24) = happyShift action_106
action_93 _ = happyFail (happyExpListPerState 93)

action_94 (24) = happyShift action_56
action_94 (25) = happyShift action_57
action_94 (15) = happyGoto action_105
action_94 _ = happyFail (happyExpListPerState 94)

action_95 _ = happyReduce_33

action_96 (60) = happyShift action_103
action_96 (62) = happyShift action_104
action_96 _ = happyReduce_50

action_97 (46) = happyShift action_102
action_97 _ = happyReduce_26

action_98 (24) = happyShift action_56
action_98 (25) = happyShift action_57
action_98 (15) = happyGoto action_101
action_98 _ = happyFail (happyExpListPerState 98)

action_99 (24) = happyShift action_56
action_99 (25) = happyShift action_57
action_99 (15) = happyGoto action_100
action_99 _ = happyFail (happyExpListPerState 99)

action_100 (24) = happyShift action_63
action_100 (18) = happyGoto action_120
action_100 _ = happyReduce_32

action_101 (24) = happyShift action_6
action_101 (25) = happyShift action_7
action_101 (17) = happyGoto action_65
action_101 (19) = happyGoto action_119
action_101 _ = happyReduce_35

action_102 (29) = happyShift action_118
action_102 _ = happyFail (happyExpListPerState 102)

action_103 (24) = happyShift action_56
action_103 (25) = happyShift action_57
action_103 (58) = happyShift action_90
action_103 (59) = happyShift action_91
action_103 (61) = happyShift action_92
action_103 (15) = happyGoto action_88
action_103 (22) = happyGoto action_117
action_103 _ = happyFail (happyExpListPerState 103)

action_104 (24) = happyShift action_56
action_104 (25) = happyShift action_57
action_104 (58) = happyShift action_90
action_104 (59) = happyShift action_91
action_104 (61) = happyShift action_92
action_104 (15) = happyGoto action_88
action_104 (22) = happyGoto action_116
action_104 _ = happyFail (happyExpListPerState 104)

action_105 (34) = happyShift action_115
action_105 _ = happyFail (happyExpListPerState 105)

action_106 (33) = happyShift action_114
action_106 _ = happyFail (happyExpListPerState 106)

action_107 (60) = happyShift action_103
action_107 (62) = happyShift action_104
action_107 _ = happyReduce_44

action_108 (24) = happyShift action_6
action_108 (25) = happyShift action_7
action_108 (17) = happyGoto action_65
action_108 (19) = happyGoto action_113
action_108 _ = happyReduce_35

action_109 (24) = happyShift action_56
action_109 (25) = happyShift action_57
action_109 (15) = happyGoto action_112
action_109 _ = happyFail (happyExpListPerState 109)

action_110 (25) = happyShift action_87
action_110 (20) = happyGoto action_111
action_110 _ = happyFail (happyExpListPerState 110)

action_111 _ = happyReduce_36

action_112 _ = happyReduce_45

action_113 (43) = happyShift action_124
action_113 _ = happyFail (happyExpListPerState 113)

action_114 (24) = happyShift action_56
action_114 (25) = happyShift action_57
action_114 (15) = happyGoto action_123
action_114 _ = happyFail (happyExpListPerState 114)

action_115 (37) = happyShift action_122
action_115 _ = happyFail (happyExpListPerState 115)

action_116 (60) = happyShift action_103
action_116 (62) = happyShift action_104
action_116 _ = happyReduce_43

action_117 (60) = happyShift action_103
action_117 (62) = happyShift action_104
action_117 _ = happyReduce_42

action_118 (24) = happyShift action_28
action_118 (11) = happyGoto action_121
action_118 _ = happyFail (happyExpListPerState 118)

action_119 _ = happyReduce_34

action_120 _ = happyReduce_31

action_121 (30) = happyShift action_128
action_121 _ = happyFail (happyExpListPerState 121)

action_122 (24) = happyShift action_56
action_122 (25) = happyShift action_57
action_122 (15) = happyGoto action_127
action_122 _ = happyFail (happyExpListPerState 122)

action_123 (34) = happyShift action_126
action_123 _ = happyFail (happyExpListPerState 123)

action_124 (24) = happyShift action_6
action_124 (25) = happyShift action_7
action_124 (17) = happyGoto action_65
action_124 (19) = happyGoto action_125
action_124 _ = happyReduce_35

action_125 (44) = happyShift action_129
action_125 _ = happyFail (happyExpListPerState 125)

action_126 _ = happyReduce_47

action_127 _ = happyReduce_48

action_128 _ = happyReduce_49

action_129 _ = happyReduce_51

happyReduce_1 = happyReduce 21 4 happyReduction_1
happyReduction_1 ((HappyTerminal (TINT _ happy_var_21)) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn19  happy_var_18) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn7  happy_var_15) `HappyStk`
	(HappyAbsSyn16  happy_var_14) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn13  happy_var_11) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn12  happy_var_8) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn6  happy_var_5) `HappyStk`
	(HappyAbsSyn5  happy_var_4) `HappyStk`
	(HappyAbsSyn17  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 ((happy_var_3,happy_var_4,happy_var_5,happy_var_8,happy_var_11,happy_var_14,happy_var_15,happy_var_18,happy_var_21)
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

happyReduce_20 = happyReduce 4 13 happyReduction_20
happyReduction_20 ((HappyAbsSyn13  happy_var_4) `HappyStk`
	(HappyAbsSyn11  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TVAR _ happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn13
		 ((happy_var_1,happy_var_3):happy_var_4
	) `HappyStk` happyRest

happyReduce_21 = happySpecReduce_0  13 happyReduction_21
happyReduction_21  =  HappyAbsSyn13
		 ([]
	)

happyReduce_22 = happySpecReduce_3  14 happyReduction_22
happyReduction_22 (HappyAbsSyn14  happy_var_3)
	_
	(HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn14
		 (happy_var_1:happy_var_3
	)
happyReduction_22 _ _ _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_1  14 happyReduction_23
happyReduction_23 (HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn14
		 ([happy_var_1]
	)
happyReduction_23 _  = notHappyAtAll 

happyReduce_24 = happyReduce 4 15 happyReduction_24
happyReduction_24 (_ `HappyStk`
	(HappyAbsSyn14  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TCONST _ happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn15
		 (Comp happy_var_1 happy_var_3
	) `HappyStk` happyRest

happyReduce_25 = happySpecReduce_1  15 happyReduction_25
happyReduction_25 (HappyTerminal (TCONST _ happy_var_1))
	 =  HappyAbsSyn15
		 (Atom happy_var_1
	)
happyReduction_25 _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_1  15 happyReduction_26
happyReduction_26 (HappyTerminal (TVAR _ happy_var_1))
	 =  HappyAbsSyn15
		 (Atom happy_var_1
	)
happyReduction_26 _  = notHappyAtAll 

happyReduce_27 = happyReduce 4 16 happyReduction_27
happyReduction_27 ((HappyAbsSyn16  happy_var_4) `HappyStk`
	(HappyAbsSyn14  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn17  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn16
		 ((happy_var_1,happy_var_3):happy_var_4
	) `HappyStk` happyRest

happyReduce_28 = happySpecReduce_3  16 happyReduction_28
happyReduction_28 (HappyAbsSyn14  happy_var_3)
	_
	(HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn16
		 ([(happy_var_1,happy_var_3)]
	)
happyReduction_28 _ _ _  = notHappyAtAll 

happyReduce_29 = happySpecReduce_1  17 happyReduction_29
happyReduction_29 (HappyTerminal (TCONST _ happy_var_1))
	 =  HappyAbsSyn17
		 (happy_var_1
	)
happyReduction_29 _  = notHappyAtAll 

happyReduce_30 = happySpecReduce_1  17 happyReduction_30
happyReduction_30 (HappyTerminal (TVAR _ happy_var_1))
	 =  HappyAbsSyn17
		 (happy_var_1
	)
happyReduction_30 _  = notHappyAtAll 

happyReduce_31 = happyReduce 7 18 happyReduction_31
happyReduction_31 ((HappyAbsSyn7  happy_var_7) `HappyStk`
	(HappyAbsSyn15  happy_var_6) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn15  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TCONST _ happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 ((happy_var_1, happy_var_3, happy_var_6):happy_var_7
	) `HappyStk` happyRest

happyReduce_32 = happyReduce 6 18 happyReduction_32
happyReduction_32 ((HappyAbsSyn15  happy_var_6) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn15  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TCONST _ happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 ([(happy_var_1, happy_var_3, happy_var_6)]
	) `HappyStk` happyRest

happyReduce_33 = happyReduce 4 19 happyReduction_33
happyReduction_33 ((HappyAbsSyn19  happy_var_4) `HappyStk`
	(HappyAbsSyn23  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn17  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn19
		 ((Local happy_var_1 happy_var_3 happy_var_4)
	) `HappyStk` happyRest

happyReduce_34 = happyReduce 6 19 happyReduction_34
happyReduction_34 ((HappyAbsSyn19  happy_var_6) `HappyStk`
	(HappyAbsSyn15  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn17  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn17  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn19
		 ((Comm happy_var_1 happy_var_3 happy_var_5 happy_var_6)
	) `HappyStk` happyRest

happyReduce_35 = happySpecReduce_0  19 happyReduction_35
happyReduction_35  =  HappyAbsSyn19
		 (End
	)

happyReduce_36 = happySpecReduce_3  20 happyReduction_36
happyReduction_36 (HappyAbsSyn20  happy_var_3)
	_
	(HappyTerminal (TVAR _ happy_var_1))
	 =  HappyAbsSyn20
		 (happy_var_1:happy_var_3
	)
happyReduction_36 _ _ _  = notHappyAtAll 

happyReduce_37 = happySpecReduce_1  20 happyReduction_37
happyReduction_37 (HappyTerminal (TVAR _ happy_var_1))
	 =  HappyAbsSyn20
		 ([happy_var_1]
	)
happyReduction_37 _  = notHappyAtAll 

happyReduce_38 = happySpecReduce_1  21 happyReduction_38
happyReduction_38 _
	 =  HappyAbsSyn21
		 (MStar
	)

happyReduce_39 = happySpecReduce_1  21 happyReduction_39
happyReduction_39 _
	 =  HappyAbsSyn21
		 (MDiamond
	)

happyReduce_40 = happySpecReduce_1  22 happyReduction_40
happyReduction_40 _
	 =  HappyAbsSyn22
		 (BTrue
	)

happyReduce_41 = happySpecReduce_1  22 happyReduction_41
happyReduction_41 _
	 =  HappyAbsSyn22
		 ((BNot BTrue)
	)

happyReduce_42 = happySpecReduce_3  22 happyReduction_42
happyReduction_42 (HappyAbsSyn22  happy_var_3)
	_
	(HappyAbsSyn22  happy_var_1)
	 =  HappyAbsSyn22
		 ((BAnd happy_var_1 happy_var_3)
	)
happyReduction_42 _ _ _  = notHappyAtAll 

happyReduce_43 = happySpecReduce_3  22 happyReduction_43
happyReduction_43 (HappyAbsSyn22  happy_var_3)
	_
	(HappyAbsSyn22  happy_var_1)
	 =  HappyAbsSyn22
		 ((BOr happy_var_1 happy_var_3)
	)
happyReduction_43 _ _ _  = notHappyAtAll 

happyReduce_44 = happySpecReduce_2  22 happyReduction_44
happyReduction_44 (HappyAbsSyn22  happy_var_2)
	_
	 =  HappyAbsSyn22
		 ((BNot happy_var_2)
	)
happyReduction_44 _ _  = notHappyAtAll 

happyReduce_45 = happySpecReduce_3  22 happyReduction_45
happyReduction_45 (HappyAbsSyn15  happy_var_3)
	_
	(HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn22
		 ((BEq happy_var_1 happy_var_3)
	)
happyReduction_45 _ _ _  = notHappyAtAll 

happyReduce_46 = happySpecReduce_2  23 happyReduction_46
happyReduction_46 (HappyAbsSyn20  happy_var_2)
	_
	 =  HappyAbsSyn23
		 ((PNew happy_var_2)
	)
happyReduction_46 _ _  = notHappyAtAll 

happyReduce_47 = happyReduce 6 23 happyReduction_47
happyReduction_47 (_ `HappyStk`
	(HappyAbsSyn15  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TCONST _ happy_var_3)) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TVAR _ happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn23
		 ((PRead happy_var_1 happy_var_3 happy_var_5)
	) `HappyStk` happyRest

happyReduce_48 = happyReduce 6 23 happyReduction_48
happyReduction_48 ((HappyAbsSyn15  happy_var_6) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn15  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TCONST _ happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn23
		 ((PWrite happy_var_1 happy_var_3 happy_var_6)
	) `HappyStk` happyRest

happyReduce_49 = happyReduce 6 23 happyReduction_49
happyReduction_49 (_ `HappyStk`
	(HappyAbsSyn11  happy_var_5) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TVAR _ happy_var_2)) `HappyStk`
	(HappyAbsSyn21  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn23
		 ((PChoice happy_var_1 happy_var_2 happy_var_5)
	) `HappyStk` happyRest

happyReduce_50 = happySpecReduce_2  23 happyReduction_50
happyReduction_50 (HappyAbsSyn22  happy_var_2)
	(HappyAbsSyn21  happy_var_1)
	 =  HappyAbsSyn23
		 ((PRelease happy_var_1 happy_var_2)
	)
happyReduction_50 _ _  = notHappyAtAll 

happyReduce_51 = happyReduce 7 23 happyReduction_51
happyReduction_51 (_ `HappyStk`
	(HappyAbsSyn19  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn19  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn22  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn23
		 ((PIf happy_var_2 happy_var_4 happy_var_6)
	) `HappyStk` happyRest

happyNewToken action sts stk [] =
	action 63 63 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TCONST _ happy_dollar_dollar -> cont 24;
	TVAR _ happy_dollar_dollar -> cont 25;
	TINT _ happy_dollar_dollar -> cont 26;
	TOPENP _ -> cont 27;
	TCLOSEP _ -> cont 28;
	TOPENB _ -> cont 29;
	TCLOSEB _ -> cont 30;
	TCOLON _ -> cont 31;
	TCOMMA _ -> cont 32;
	TOPENSQB _ -> cont 33;
	TCLOSESQB _ -> cont 34;
	TDIAMOND _ -> cont 35;
	TSTAR _ -> cont 36;
	TASSIGN _ -> cont 37;
	TEQUAL _ -> cont 38;
	TSLASH _ -> cont 39;
	TCHANNEL _ -> cont 40;
	TIF _ -> cont 41;
	TTHEN _ -> cont 42;
	TELSE _ -> cont 43;
	TEND _ -> cont 44;
	TNEW _ -> cont 45;
	TIN _ -> cont 46;
	TDISHONEST _ -> cont 47;
	TPRIVATE _ -> cont 48;
	TPROTOCOL _ -> cont 49;
	TSIGMA0 _ -> cont 50;
	TSIGMA _ -> cont 51;
	TCELLS _ -> cont 52;
	TAGENTS _ -> cont 53;
	TROLES _ -> cont 54;
	TKNOWLEDGE _ -> cont 55;
	TACIONS _ -> cont 56;
	TBOUND _ -> cont 57;
	TTRUE _ -> cont 58;
	TFALSE _ -> cont 59;
	TAND _ -> cont 60;
	TNOT _ -> cont 61;
	TOR _ -> cont 62;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 63 tk tks = happyError' (tks, explist)
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
