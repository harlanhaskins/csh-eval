{-|
Module      : CSH.Eval.Model
Description : CSH Eval state representation data structures.
Copyright   : Stephen Demos, Matt Gambogi, Travis Whitaker, Computer Science House 2015
License     : MIT
Maintainer  : pvals@csh.rit.edu
Stability   : Provisional
Portability : POSIX

CSH.Eval.Model defines all data structures required for representing and
manipulating the system state.
-}

module CSH.Eval.Model (
    -- * /Enum/-like Types
    Committee(..)
  , EvaluationType(..)
  , EvaluationStatus(..)
  , MemberStatus(..)
  , DuesStatus(..)
  , EventType(..)
  , ProjectType(..)

    -- * Top-level Objects
  , Member(..)
  , Event(..)
  , Project(..)
  , Evaluation(..)
  , Conditional(..)
  , FreshmanProject(..)
  , Packet(..)
  , Queue(..)
  , Application(..)
  , Metric(..)
  , Review(..)
  , Interview(..)
  , Question(..)
  , Term(..)

    -- * Contexts
  , Eboard(..)
  , Room(..)
  , Membership(..)
  , EventAttendee(..)
  , ProjectParticipant(..)
  , FreshmanProjectParticipant(..)
  , Signature(..)
  , ReviewMetric(..)
  , InterviewMetric(..)
  , Answer(..)
  , Dues(..)

  -- * Cache
  , IDCache
  , Cache(..)
  , defTxMode
  , CacheM(..)
  , Cacheable(..)
  , CacheError(..)
  , runCacheable
  , execCacheable
  ) where

import Control.Concurrent.MVar

import Control.Monad.Trans.Either

import Data.Maybe

import Data.List

import Data.Time.Clock
import Data.Time.Calendar

import Data.UUID

import Data.Word

import qualified Data.ByteString as B

import qualified Data.Text       as T

import qualified Data.Map        as M

import Hasql
import Hasql.Postgres

data Committee = Evals
               | RnD
               | Social
               | History
               | OpCOmm
               | Imps
               | Financial
               | Chairman
               deriving (Eq, Show)

data EvaluationType = IntroEval
                    | MembershipEval
                    deriving (Eq, Show)

data EvaluationStatus = Pending
                      | Passed
                      | Failed
                      deriving (Eq, Show)

data MemberStatus = Active
                  | AlumniGood
                  | AlumniBad
                  | Honorary
                  | Advisory
                  | Intro
                  | Non
                  deriving (Eq, Show)

data DuesStatus = Paid
                | Exempt
                deriving (Eq, Show)

data EventType = HouseMeeting
               | SocialEvent
               | CommitteeMeeting
               | Seminar
               | Orientation
               deriving (Eq, Show)

data ProjectType = Major
                 deriving (Eq, Show)

-- | Construct an equality function for a record type based on the value of a
--   single constructor.
mkEq :: Eq b => (a -> b) -> a -> a -> Bool
mkEq f = (\ a b -> (f a) == (f b))

data Member = Member {
    memberID            :: Word64
  , memberUUID          :: UUID
  , memberUsername      :: T.Text
  , memberCommonname    :: T.Text
  , memberToken         :: Maybe B.ByteString
  , memberHousingPoints :: Int
  , memberOnfloorStatus :: Bool
  , memberEboards       :: Cacheable [Eboard]
  , memberRooms         :: Cacheable [Room]
  , memberMemberships   :: Cacheable [Membership]
  , memberEvaluations   :: Cacheable [Evaluation]
  , memberPackets       :: Cacheable [Packet]
  , memberQueues        :: Cacheable [Queue]
  , memberApplications  :: Cacheable [Application]
  , memberDues          :: Cacheable [Dues]
  }

instance Eq Member where
    (==) = mkEq memberID

instance Show Member where
    show m = intercalate "\n"
        [ ("Member ID: " ++ (show $ memberID m))
        , ("Member UUID: " ++ (show $ memberUUID m))
        , ("Member Username: " ++ (show $ memberUsername m))
        , ("Member Common Name: " ++ (show $ memberCommonname m))
        , ("Member Logged In: " ++ (if (isJust $ memberToken m) then "True" else "False"))
        , ("Member Housing Points: " ++ (show $ memberHousingPoints m))
        , ("Member Onfloor Status: " ++ (show $ memberOnfloorStatus m))
        ]

data Event = Event {
    eventID          :: Word64
  , eventTitle       :: T.Text
  , eventHeld        :: UTCTime
  , eventCategory    :: EventType
  , eventCommittee   :: Committee
  , eventDescription :: T.Text
  , eventAttendees   :: Cacheable [EventAttendee]
  }

instance Eq Event where
    (==) = mkEq eventID

instance Show Event where
    show e = intercalate "\n"
        [ ("Event ID: " ++ (show $ eventID e))
        , ("Event Title: " ++ (show $ eventTitle e))
        , ("Event Held: " ++ (show $ eventHeld e))
        , ("Event Category: " ++ (show $ eventCategory e))
        , ("Event Committee: " ++ (show $ eventCommittee e))
        , ("Event Description: " ++ (show $ eventDescription e))
        ]

data Project = Project {
    projectID           :: Word64
  , projectTitle        :: T.Text
  , projectDescription  :: T.Text
  , projectSubmitted    :: UTCTime
  , projectPassed       :: Maybe UTCTime
  , projectCommittee    :: Committee
  , projectType         :: ProjectType
  , projectComments     :: T.Text
  , projectStatus       :: EvaluationStatus
  , projectParticipants :: Cacheable [ProjectParticipant]
  }

instance Eq Project where
    (==) = mkEq projectID

instance Show Project where
    show p = intercalate "\n"
        [ ("Project ID: " ++ (show $ projectID p))
        , ("Project Title: " ++ (show $ projectTitle p))
        , ("Project Description: " ++ (show $ projectDescription p))
        , ("Project Submitted: " ++ (show $ projectSubmitted p))
        , ("Project Passed: " ++ (show $ projectPassed p))
        , ("Project Committee: " ++ (show $ projectCommittee p))
        , ("Project Type: " ++ (show $ projectType p))
        , ("Project Comments: " ++ (show $ projectComments p))
        , ("Project Status: " ++ (show $ projectStatus p))
        ]

data Evaluation = Evaluation {
    evaluationID           :: Word64
  , evaluationComments     :: T.Text
  , evaluationDeadline     :: UTCTime
  , evaluationAvailable    :: Bool
  , evaluationStatus       :: EvaluationStatus
  , evaluationType         :: EvaluationType
  , evaluationMember       :: Member
  , evaluationConditionals :: Cacheable [Conditional]
  , evaluationFreshProject :: Cacheable [FreshmanProjectParticipant]
  }

instance Eq Evaluation where
    (==) = mkEq evaluationID

instance Show Evaluation where
    show e = intercalate "\n"
        [ ("Evaluation ID: " ++ (show $ evaluationID e))
        , ("Evaluation Comments: " ++ (show $ evaluationComments e))
        , ("Evaluation Deadline: " ++ (show $ evaluationDeadline e))
        , ("Evaluation Available: " ++ (show $ evaluationAvailable e))
        , ("Evaluation Status: " ++ (show $ evaluationStatus e))
        , ("Evaluation Type: " ++ (show $ evaluationType e))
        ]

data Conditional = Conditional {
    conditionalID          :: Word64
  , conditionalDeadline    :: UTCTime
  , conditionalDescription :: T.Text
  , conditionalComments    :: T.Text
  }

instance Eq Conditional where
    (==) = mkEq conditionalID

instance Show Conditional where
    show c = intercalate "\n"
        [ ("Conditional ID: " ++ (show $ conditionalID c))
        , ("Conditional Deadline: " ++ (show $ conditionalDeadline c))
        , ("Conditional Description: " ++ (show $ conditionalDescription c))
        , ("Conditional Comments: " ++ (show $ conditionalComments c))
        ]

data FreshmanProject = FreshmanProject {
    freshmanProjectID           :: Word64
  , freshmanProjectDescription  :: T.Text
  , freshmanProjectTerm         :: Term
  , freshmanProjectEvent        :: Event
  , freshmanProjectParticipants :: Cacheable [FreshmanProjectParticipant]
  }

instance Eq FreshmanProject where
    (==) = mkEq freshmanProjectID

instance Show FreshmanProject where
    show f = intercalate "\n"
        [ ("Freshman Project ID: " ++ (show $ freshmanProjectID f))
        , ("Freshman Project Description: " ++ (show $ freshmanProjectDescription f))
        ]

data Packet = Packet {
    packetID         :: Word64
  , packetDueDate    :: Day
  , packetPercentReq :: Integer
  , packetMember     :: Member
  , packetSignatures :: Cacheable [Signature]
  }

instance Eq Packet where
    (==) = mkEq packetID

instance Show Packet where
    show p = intercalate "\n"
        [ ("Packet ID: " ++ (show $ packetID p))
        , ("Packet Due Date: " ++ (show $ packetDueDate p))
        , ("Packet Percent Required: " ++ (show $ packetPercentReq p))
        ]

-- This should not be here...
data Queue = Queue {
    queueEntered :: UTCTime
  , queueExited  :: Maybe UTCTime
  , queueMember  :: Member
  }

instance Show Queue where
    show q = intercalate "\n"
        [ ("Queue Entered: " ++ (show $ queueEntered q))
        , ("Queue Exited: " ++ (show $ queueExited q))
        ]

data Application = Application {
    applicationID        :: Word64
  , applicationCreated   :: UTCTime
  , applicationStatus    :: EvaluationStatus
  , applicationMember    :: Member
  , applicationReviews   :: Cacheable [Review]
  , applicationInterview :: Cacheable [Interview]
  , applicationAnswers   :: Cacheable [Answer]
  }

instance Eq Application where
    (==) = mkEq applicationID

instance Show Application where
    show a = intercalate "\n"
        [ ("Application ID: " ++ (show $ applicationID a))
        , ("Application Created: " ++ (show $ applicationCreated a))
        , ("Application Status: " ++ (show $ applicationStatus a))
        ]

data Metric = Metric {
    metricID     :: Word64
  , metricName   :: T.Text
  , metricActive :: Bool
  }

instance Eq Metric where
    (==) = mkEq metricID

instance Show Metric where
    show m = intercalate "\n"
        [ ("Metric ID: " ++ (show $ metricID m))
        , ("Metric Name: " ++ (show $ metricName m))
        , ("Metric Active: " ++ (show $ metricActive m))
        ]

data Review = Review {
    reviewID          :: Word64
  , reviewStart       :: UTCTime
  , reviewSubmit      :: UTCTime
  , reviewMember      :: Member
  , reviewApplication :: Application
  , reviewMetrics     :: Cacheable [ReviewMetric]
  }

instance Eq Review where
    (==) = mkEq reviewID

instance Show Review where
    show r = intercalate "\n"
        [ ("Review ID: " ++ (show $ reviewID r))
        , ("Review Start: " ++ (show $ reviewStart r))
        , ("Review Submit: " ++ (show $ reviewSubmit r))
        ]

data Interview = Interview {
    interviewID          :: Word64
  , interviewDate        :: UTCTime
  , interviewMember      :: Member
  , interviewApplication :: Application
  , interviewMetrics     :: Cacheable [InterviewMetric]
  }

instance Eq Interview where
    (==) = mkEq interviewID

instance Show Interview where
    show i = intercalate "\n"
        [ ("Interview ID: " ++ (show $ interviewID i))
        , ("Interview Date: " ++ (show $ interviewDate i))
        ]

data Question = Question {
    questionID     :: Word64
  , questionQuery  :: T.Text
  , questionActive :: Bool
  }

instance Eq Question where
    (==) = mkEq questionID

instance Show Question where
    show q = intercalate "\n"
        [ ("Question ID: " ++ (show $ questionID q))
        , ("Question Query: " ++ (show $ questionQuery q))
        , ("Question Active: " ++ (show $ questionActive q))
        ]

data Term = Term {
    termID        :: Word64
  , termStartDate :: Day
  , termEndDate   :: Maybe Day
  }

instance Eq Term where
    (==) = mkEq termID

instance Show Term where
    show t = intercalate "\n"
        [ ("Term ID: " ++ (show $ termID t))
        , ("Term Start Date: " ++ (show $ termStartDate t))
        , ("Term End Date: " ++ (show $ termEndDate t))
        ]

data Eboard = Eboard {
    eboardCommittee :: Committee
  , eboardStartDate :: Day
  , eboardEndDate   :: Maybe Day
  , eboardMember    :: Member
  }

instance Show Eboard where
    show e = intercalate "\n"
        [ ("Eboard Committee: " ++ (show $ eboardCommittee e))
        , ("Eboard Start Date: " ++ (show $ eboardStartDate e))
        , ("Eboard End Date: " ++ (show $ eboardEndDate e))
        ]

data Room = Room {
    roomNumber    :: T.Text
  , roomStartDate :: Day
  , roomEndDate   :: Day
  , roomMember    :: Member
  }

instance Show Room where
    show r = intercalate "\n"
        [ ("Room Number: " ++ (show $ roomNumber r))
        , ("Room Start Date: " ++ (show $ roomStartDate r))
        , ("Room End Date: " ++ (show $ roomEndDate r))
        ]

data Membership = Membership {
    membershipStatus    :: MemberStatus
  , membershipStartDate :: Day
  , membershipEndDate   :: Maybe Day
  , membershipMember    :: Member
  }

instance Show Membership where
    show m = intercalate "\n"
        [ ("Membership Status: " ++ (show $ membershipStatus m))
        , ("Membership Start Date: " ++ (show $ membershipStartDate m))
        , ("Membership End Date: " ++ (show $ membershipEndDate m))
        ]

data EventAttendee = EventAttendee {
   eventAttendeeHost   :: Bool
 , eventAttendeeMember :: Member
 , eventAttendeeEvent  :: Event
 }

instance Show EventAttendee where
    show e = intercalate "\n"
        [ ("Event Attendee Host: " ++ (show $ eventAttendeeHost e))
        ]

data ProjectParticipant = ProjectParticipant {
    projectParticipantDescription :: T.Text
  , projectParticipantMember      :: Member
  , projectParticipantProject     :: Project
  }

instance Show ProjectParticipant where
    show p = intercalate "\n"
        [ ("Project Participant Description: " ++ (show $ projectParticipantDescription p))
        ]

data FreshmanProjectParticipant = FreshmanProjectParticipant {
    freshmanProjectParticipantEboard     :: Bool
  , freshmanProjectParticipantStatus     :: EvaluationStatus
  , freshmanProjectParticipantComments   :: T.Text
  , freshmanProjectParticipantFreshProj  :: FreshmanProject
  , freshmanProjectParticipantEvaluation :: Evaluation
  }

instance Show FreshmanProjectParticipant where
    show f = intercalate "\n"
        [ ("Freshman Project Participant Eboard: " ++ (show $ freshmanProjectParticipantEboard f))
        , ("Freshman Project Participant Status: " ++ (show $ freshmanProjectParticipantStatus f))
        , ("Freshman Project Participant Comments: " ++ (show $ freshmanProjectParticipantComments f))
        ]

data Signature = Signature {
    signatureRequired :: Bool
  , signatureSigned   :: Maybe UTCTime
  , signatureMember   :: Member
  , signaturePacket   :: Packet
  }

instance Show Signature where
    show s = intercalate "\n"
        [ ("Signature Required: " ++ (show $ signatureRequired s))
        , ("Signature Signed: " ++ (show $ signatureSigned s))
        ]

data ReviewMetric = ReviewMetric {
    reviewMetricScore  :: Integer
  , reviewMetricMetric :: Metric
  , reviewMetricReview :: Review
  }

instance Show ReviewMetric where
    show r = intercalate "\n"
        [ ("Review Metric Score: " ++ (show $ reviewMetricScore r))
        ]

data InterviewMetric = InterviewMetric {
    interviewMetricScore     :: Integer
  , interviewMetricMetric    :: Metric
  , interviewMetricInterview :: Interview
  }

instance Show InterviewMetric where
    show i = intercalate "\n"
        [ ("Interview Metric Score: " ++ (show $ interviewMetricScore i))
        ]

data Answer = Answer {
    answerResponse    :: T.Text
  , answerQuestion    :: Question
  , answerApplication :: Application
  }

instance Show Answer where
    show a = intercalate "\n"
        [ ("Answer Response: " ++ (show $ answerResponse a))
        ]

data Dues = Dues {
    duesStatus :: DuesStatus
  , duesMember :: Member
  , duesTerm   :: Term
  }

instance Show Dues where
    show d = intercalate "\n"
        [ ("Dues Status: " ++ (show $ duesStatus d))
        ]

-- | A cache segment from database IDs to objects.
type IDCache a = MVar (M.Map Word64 (MVar a))

-- | A cache state, consisting of all cache segments and a database connection
--   pool for fallbacks (this must be accessible within the 'Cacheable' monad).
data Cache = Cache {
    memberIDCache                         :: IDCache Member
  , eventIDCache                          :: IDCache Event
  , projectIDCache                        :: IDCache Project
  , evaluationIDCache                     :: IDCache Evaluation
  , conditionalIDCache                    :: IDCache Conditional
  , freshmanProjectIDCache                :: IDCache FreshmanProject
  , packetIDCache                         :: IDCache Packet
  , queueIDCache                          :: IDCache Queue
  , applicationIDCache                    :: IDCache Application
  , metricIDCache                         :: IDCache Metric
  , reviewIDCache                         :: IDCache Review
  , interviewIDCache                      :: IDCache Interview
  , questionIDCache                       :: IDCache Question
  , termIDCache                           :: IDCache Term
  , eboardMemberIDCache                   :: IDCache [Eboard]
  , roomMemberIDCache                     :: IDCache [Room]
  , membershipMemberIDCache               :: IDCache [Membership]
  , eventAttendeeMemberIDCache            :: IDCache [EventAttendee]
  , eventAttendeeEventIDCache             :: IDCache [EventAttendee]
  , projectParticipantMemberIDCache       :: IDCache [ProjectParticipant]
  , projectParticipantProjectIDCache      :: IDCache [ProjectParticipant]
  , freshProjParticipantProjectIDCache    :: IDCache [FreshmanProjectParticipant]
  , freshProjParticipantEvaluationIDCache :: IDCache [FreshmanProjectParticipant]
  , signatureMemberIDCache                :: IDCache [Signature]
  , signaturePacketIDCache                :: IDCache [Signature]
  , reviewMetricMetricIDCache             :: IDCache [ReviewMetric]
  , reviewMetricReviewIDCache             :: IDCache [ReviewMetric]
  , interviewMetricMetricIDCache          :: IDCache [InterviewMetric]
  , interviewMetricInterviewIDCache       :: IDCache [InterviewMetric]
  , answerQuestionIDCache                 :: IDCache [Answer]
  , answerApplicationIDCache              :: IDCache [Answer]
  , duesMemberIDCache                     :: IDCache [Dues]
  , pool                                  :: Pool Postgres
  }

-- | Default Hasql transaction mode.
defTxMode :: TxMode
defTxMode = Just (Serializable, (Just True))

-- | Interior transformer for operations directly on the cache state. Cache
--   users should never be able to bind out of this.
type CacheM a = EitherT CacheError IO a

-- | Exterior transformer for cache operations. Cache API caller-facind
--   functions /must/ return into this exterior transformer.
type Cacheable a = Cache -> CacheM a

-- | Cache error.
data CacheError = HasqlError (SessionError Postgres)
                | CacheError  String
                | Nonexistent String
                | Constraint  String

-- | Enables embedding the interior cache transformer within another
--   transformer. This is only OK to use if you're embedding something
--   'Cacheable' in a different exterior transformer. I'm not sure how to
--   enforce that at the type level. You probably shouldn't use this.
runCacheable :: Cache -> Cacheable a -> CacheM a
runCacheable c m = m c

-- | Hoist 'Cacheable' into IO.
execCacheable :: Cache -> Cacheable a -> IO (Either CacheError a)
execCacheable c m = runEitherT (m c)
