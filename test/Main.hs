module Main where

import Helpers
import System.Exit (ExitCode(..), exitSuccess, exitWith)
import Test.HUnit
import Text.HTML.TagSoup

tests :: Test
tests =
  test
    [ "test1" ~: "A number, from tags" ~: Just "A Number" ~=?
      maybeTagText (getNumber tags)
    , "test2" ~: "A title, from tags" ~: Just "A Title" ~=?
      maybeTagText (getTitle tags)
    , "test3" ~: "The points, from tags" ~: Just "The Points" ~=?
      maybeTagText (getPoints tags)
    , "test4" ~: "The comments, from tags" ~: Just "The Comments count" ~=?
      maybeTagText (getCommentsCount tags)
    , "test5" ~: "Tag to number" ~: Just "A Number" ~=? tagToNumber tags
    , "test6" ~: "Tag to title" ~: Just "A Title" ~=? tagToTitle tags
    , "test7" ~: "Tag to points" ~: Just "The Points" ~=? tagToPoints tags
    , "test8" ~: "Tag to comment count" ~: Just "The Comments count" ~=?
      tagToCommentsCount tags
    , "test9" ~: "Make an Article from tags" ~:
      "Article (Just \"1.\") (Just \"End of the Road: An AnandTech Farewell\") (Just 725) (Just 157)" ~=?
      show (makeArticle mockArticle)
    , "test10" ~: "Expect 30 articles" ~: 30 ~=?
      (length $ narrowTags hackerNewsSample)
    , "test11" ~: "find number" ~: Just "1." ~=? findNumber mockArticle
    , "test12" ~: "find title" ~: Just "End of the Road: An AnandTech Farewell" ~=?
      findTitle mockArticle
    , "test13" ~: "find points" ~: Just "725 points" ~=? findPoints mockArticle
    , "test14" ~: "find comments count" ~: Just "157\160comments" ~=?
      findCommentsCount mockArticle
     , "test15" ~: "A list of lists of articles" ~: 30 ~=?
      (length $ allArticles (narrowTags hackerNewsSample))
     , "test16" ~: "numericCount with Int" ~: Just 12 ~=? (numericCount $ (Just "12 comments"))
     , "test17" ~: "numericCount with no Int" ~: Nothing ~=? (numericCount $ (Just "comments"))
    ]

tags :: [Tag String]
tags =
  [ TagText "A Number"
  , TagText "A Title"
  , TagText "The Points"
  , TagText "The Comments count"
  ]

mockArticle :: [Tag String]
mockArticle = (head (narrowTags hackerNewsSample))

main :: IO ()
main = do
  n <- runTestTT tests
  exit $ failures n

exit :: Int -> IO ()
exit 0 = exitSuccess
exit n = exitWith $ ExitFailure n

-- Example of Hacker News content.
hackerNewsSample :: String
hackerNewsSample =
  "<!DOCTYPE html>\
 \<html lang='en' op='news'>\
 \<head>\
 \  <meta name='generator' content='HTML Tidy for HTML5 for Linux version 5.8.0'>\
 \  <meta name='referrer' content='origin'>\
 \  <meta name='viewport' content='width=device-width, initial-scale=1.0'>\
 \  <link rel='stylesheet' type='text/css' href='news.css?2QLhhCRy3pNA8Rfan1sV'>\
 \  <link rel='icon' href='y18.svg'>\
 \  <link rel='alternate' type='application/rss+xml' title='RSS' href='rss'>\
 \  <title>Hacker News</title>\
 \</head>\
 \<body>\
 \  <center>\
 \    <table id='hnmain' border='0' cellpadding='0' cellspacing='0' width='85%' bgcolor='#F6F6EF'>\
 \      <tr>\
 \        <td bgcolor='#FF6600'>\
 \          <table border='0' cellpadding='0' cellspacing='0' width='100%' style='padding:2px'>\
 \            <tr>\
 \              <td style='width:18px;padding-right:4px'>\
 \                <a href='https://news.ycombinator.com'><img src='y18.svg' width='18' height='18' style='border:1px white solid; display:block'></a>\
 \              </td>\
 \              <td style='line-height:12pt; height:10px;'><span class='pagetop'><b class='hnname'><a href='news'>Hacker News</a></b> <a href='newest'>new</a> | <a href='front'>past</a> | <a href='newcomments'>comments</a> | <a href='ask'>ask</a> | <a href='show'>show</a> | <a href='jobs'>jobs</a> | <a href='submit' rel='nofollow'>submit</a></span></td>\
 \              <td style='text-align:right;padding-right:4px;'><span class='pagetop'><a href='login?goto=news'>login</a></span></td>\
 \            </tr>\
 \          </table>\
 \        </td>\
 \      </tr>\
 \      <tr id='pagespace' title='' style='height:10px'>\
 \        <td></td>\
 \      </tr>\
 \      <tr>\
 \        <td>\
 \          <table border='0' cellpadding='0' cellspacing='0'>\
 \            <tr class='athing' id='41399872'>\
 \              <td align='right' valign='top' class='title'><span class='rank'>1.</span></td>\
 \              <td valign='top' class='votelinks'>\
 \                <center>\
 \                  <a id='up_41399872' href='vote?id=41399872&amp;how=up&amp;goto=news'>\
 \                  <div class='votearrow' title='upvote'></div></a>\
 \                </center>\
 \              </td>\
 \              <td class='title'><span class='titleline'><a href='https://www.anandtech.com/show/21542/end-of-the-road-an-anandtech-farewell'>End of the Road: An AnandTech Farewell</a> <span class='sitebit comhead'>(<a href='from?site=anandtech.com'><span class='sitestr'>anandtech.com</span></a>)</span></span></td>\
 \            </tr>\
 \            <tr>\
 \              <td colspan='2'></td>\
 \              <td class='subtext'><span class='subline'><span class='score' id='score_41399872'>725 points</span> by <a href='user?id=janice1999' class='hnuser'>janice1999</a> <span class='age' title='2024-08-30T12:05:23'><a href='ite%20?id=41399872'>2 hours ago</a></span> <span id='unv_41399872'></span> | <a href='hide?id=41399872&amp;goto=news'>hide</a> | <a href='item?id=41399872'>157&nbsp;comments</a></span></td>\
 \            </tr>\
 \            <tr class='spacer' style='height:5px'>\
 \              <td></td>\
 \            </tr>\
 \            <tr class='athing' id='41395413'>\
 \              <td align='right' valign='top' class='title'><span class='rank'>2.</span></td>\
 \              <td valign='top' class='votelinks'>\
 \                <center>\
 \                  <a id='up_41395413' href='vote?id=41395413&amp;how=up&amp;goto=news'>\
 \                  <div class='votearrow' title='upvote'></div></a>\
 \                </center>\
 \              </td>\
 \              <td class='title'><span class='titleline'><a href='https://eieio.games/essays/the-secret-in-one-million-checkboxes/'>The secret inside One Million Checkboxes</a> <span class='sitebit comhead'>(<a href='from?site=eieio.games'><span class='sitestr'>eieio.games</span></a>)</span></span></td>\
 \            </tr>\
 \            <tr>\
 \              <td colspan='2'></td>\
 \              <td class='subtext'><span class='subline'><span class='score' id='score_41395413'>1708 points</span> by <a href='user?id=todsacerdoti' class='hnuser'>todsacerdoti</a> <span class='age' title='2024-08-29T21:20:12'><a href='item?id=41395413'>17 hours ago</a></span> <span id='unv_41395413'></span> | <a href='hide?id=41395413&amp;goto=news'>hide</a> | <a href='item?id=41395413'>154&nbsp;comments</a></span></td>\
 \            </tr>\
 \            <tr class='spacer' style='height:5px'>\
 \              <td></td>\
 \            </tr>\
 \            <tr class='athing' id='41357123'>\
 \              <td align='right' valign='top' class='title'><span class='rank'>3.</span></td>\
 \              <td valign='top' class='votelinks'>\
 \                <center>\
 \                  <a id='up_41357123' href='vote?id=41357123&amp;how=up&amp;goto=news'>\
 \                  <div class='votearrow' title='upvote'></div></a>\
 \                </center>\
 \              </td>\
 \              <td class='title'><span class='titleline'><a href='https://www.morling.dev/blog/leader-election-with-s3-conditional-writes/'>Leader Election with S3 Conditional Writes</a> <span class='sitebit comhead'>(<a href='from?site=morling.dev'><span class='sitestr'>morling.dev</span></a>)</span></span></td>\
 \            </tr>\
 \            <tr>\
 \              <td colspan='2'></td>\
 \              <td class='subtext'><span class='subline'><span class='score' id='score_41357123'>27 points</span> by <a href='user?id=gunnarmorling' class='hnuser'>gunnarmorling</a> <span class='age' title='2024-08-26T13:54:08'><a href='item?id=41357123'>2 hours ago</a></span> <span id='unv_41357123'></span> | <a href='hide?id=41357123&amp;goto=news'>hide</a> | <a href='item?id=41357123'>5&nbsp;comments</a></span></td>\
 \            </tr>\
 \            <tr class='spacer' style='height:5px'>\
 \              <td></td>\
 \            </tr>\
 \            <tr class='athing' id='41398925'>\
 \              <td align='right' valign='top' class='title'><span class='rank'>4.</span></td>\
 \              <td valign='top' class='votelinks'>\
 \                <center>\
 \                  <a id='up_41398925' href='vote?id=41398925&amp;how=up&amp;goto=news'>\
 \                  <div class='votearrow' title='upvote'></div></a>\
 \                </center>\
 \              </td>\
 \              <td class='title'><span class='titleline'><a href='https://digitalpreservation-blog.nb.no/blog/2024-08-28-rearchiving-2-million-hours-of-digital-radio/'>Rearchiving 2M hours of digital radio, a comprehensive process</a> <span class='sitebit comhead'>(<a href='from?site=nb.no'><span class='sitestr'>nb.no</span></a>)</span></span></td>\
 \            </tr>\
 \            <tr>\
 \              <td colspan='2'></td>\
 \              <td class='subtext'><span class='subline'><span class='score' id='score_41398925'>80 points</span> by <a href='user?id=todsacerdoti' class='hnuser'>todsacerdoti</a> <span class='age' title='2024-08-30T08:59:07'><a href='item?id=41398925'>5 hours ago</a></span> <span id='unv_41398925'></span> | <a href='hide?id=41398925&amp;goto=news'>hide</a> | <a href='item?id=41398925'>22&nbsp;comments</a></span></td>\
 \            </tr>\
 \            <tr class='spacer' style='height:5px'>\
 \              <td></td>\
 \            </tr>\
 \            <tr class='athing' id='41392128'>\
 \              <td align='right' valign='top' class='title'><span class='rank'>5.</span></td>\
 \              <td valign='top' class='votelinks'>\
 \                <center>\
 \                  <a id='up_41392128' href='vote?id=41392128&amp;how=up&amp;goto=news'>\
 \                  <div class='votearrow' title='upvote'></div></a>\
 \                </center>\
 \              </td>\
 \              <td class='title'><span class='titleline'><a href='https://ian.sh/tsa'>Bypassing airport security via SQL injection</a> <span class='sitebit comhead'>(<a href='from?site=ian.sh'><span class='sitestr'>ian.sh</span></a>)</span></span></td>\
 \            </tr>\
 \            <tr>\
 \              <td colspan='2'></td>\
 \              <td class='subtext'><span class='subline'><span class='score' id='score_41392128'>1764 points</span> by <a href='user?id=iancarroll' class='hnuser'>iancarroll</a> <span class='age' title='2024-08-29T15:53:08'><a href='item?id=41392128'>22 hours ago</a></span> <span id='unv_41392128'></span> | <a href='hide?id=41392128&amp;goto=news'>hide</a> | <a href='item?id=41392128'>368&nbsp;comments</a></span></td>\
 \            </tr>\
 \            <tr class='spacer' style='height:5px'>\
 \              <td></td>\
 \            </tr>\
 \            <tr class='athing' id='41368583'>\
 \              <td align='right' valign='top' class='title'><span class='rank'>6.</span></td>\
 \              <td valign='top' class='votelinks'>\
 \                <center>\
 \                  <a id='up_41368583' href='vote?id=41368583&amp;how=up&amp;goto=news'>\
 \                  <div class='votearrow' title='upvote'></div></a>\
 \                </center>\
 \              </td>\
 \              <td class='title'><span class='titleline'><a href='https://www.bly.com/Pages/documents/STIKFS.html'>Marketing to Engineers</a> <span class='sitebit comhead'>(<a href='from?site=bly.com'><span class='sitestr'>bly.com</span></a>)</span></span></td>\
 \            </tr>\
 \            <tr>\
 \              <td colspan='2'></td>\
 \              <td class='subtext'><span class='subline'><span class='score' id='score_41368583'>165 points</span> by <a href='user?id=herbertl' class='hnuser'>herbertl</a> <span class='age' title='2024-08-27T15:23:04'><a href='item?id=41368583'>7 hours ago</a></span> <span id='unv_41368583'></span> | <a href='hide?id=41368583&amp;goto=news'>hide</a> | <a href='item?id=41368583'>128&nbsp;comments</a></span></td>\
 \            </tr>\
 \            <tr class='spacer' style='height:5px'>\
 \              <td></td>\
 \            </tr>\
 \            <tr class='athing' id='41376893'>\
 \              <td align='right' valign='top' class='title'><span class='rank'>7.</span></td>\
 \              <td valign='top' class='votelinks'>\
 \                <center>\
 \                  <a id='up_41376893' href='vote?id=41376893&amp;how=up&amp;goto=news'>\
 \                  <div class='votearrow' title='upvote'></div></a>\
 \                </center>\
 \              </td>\
 \              <td class='title'><span class='titleline'><a href='https://liorsinai.github.io/machine-learning/2024/07/27/micrograd-1-chainrules.html'>Micrograd.jl</a> <span class='sitebit comhead'>(<a href='from?site=liorsinai.github.io'><span class='sitestr'>liorsinai.github.io</span></a>)</span></span></td>\
 \            </tr>\
 \            <tr>\
 \              <td colspan='2'></td>\
 \              <td class='subtext'><span class='subline'><span class='score' id='score_41376893'>89 points</span> by <a href='user?id=the_origami_fox' class='hnuser'>the_origami_fox</a> <span class='age' title='2024-08-28T07:24:42'><a href='item?id=41376893'>7 hours ago</a></span> <span id='unv_41376893'></span> | <a href='hide?id=41376893&amp;goto=news'>hide</a> | <a href='item?id=41376893'>17&nbsp;comments</a></span></td>\
 \            </tr>\
 \            <tr class='spacer' style='height:5px'>\
 \              <td></td>\
 \            </tr>\
 \            <tr class='athing' id='41398351'>\
 \              <td align='right' valign='top' class='title'><span class='rank'>8.</span></td>\
 \              <td valign='top' class='votelinks'>\
 \                <center>\
 \                  <a id='up_41398351' href='vote?id=41398351&amp;how=up&amp;goto=news'>\
 \                  <div class='votearrow' title='upvote'></div></a>\
 \                </center>\
 \              </td>\
 \              <td class='title'><span class='titleline'><a href='https://www.iacr.org/cryptodb/data/paper.php?pubkey=34281'>Time-Memory Trade-Offs Sound the Death Knell for GPRS and GSM</a> <span class='sitebit comhead'>(<a href='from?site=iacr.org'><span class='sitestr'>iacr.org</span></a>)</span></span></td>\
 \            </tr>\
 \            <tr>\
 \              <td colspan='2'></td>\
 \              <td class='subtext'><span class='subline'><span class='score' id='score_41398351'>58 points</span> by <a href='user?id=belter' class='hnuser'>belter</a> <span class='age' title='2024-08-30T07:06:34'><a href='item?id=41398351'>7 hours ago</a></span> <span id='unv_41398351'></span> | <a href='hide?id=41398351&amp;goto=news'>hide</a> | <a href='item?id=41398351'>28&nbsp;comments</a></span></td>\
 \            </tr>\
 \            <tr class='spacer' style='height:5px'>\
 \              <td></td>\
 \            </tr>\
 \            <tr class='athing' id='41400807'>\
 \              <td align='right' valign='top' class='title'><span class='rank'>9.</span></td>\
 \              <td valign='top' class='votelinks'>\
 \                <center>\
 \                  <a id='up_41400807' href='vote?id=41400807&amp;how=up&amp;goto=news'>\
 \                  <div class='votearrow' title='upvote'></div></a>\
 \                </center>\
 \              </td>\
 \              <td class='title'><span class='titleline'><a href='https://grist.org/transportation/the-forgotten-fight-to-ban-gas-powered-cars-in-the-1960s/'>The fight to ban gas-powered cars in the 1960s</a> <span class='sitebit comhead'>(<a href='from?site=grist.org'><span class='sitestr'>grist.org</span></a>)</span></span></td>\
 \            </tr>\
 \            <tr>\
 \              <td colspan='2'></td>\
 \              <td class='subtext'><span class='subline'><span class='score' id='score_41400807'>12 points</span> by <a href='user?id=robtherobber' class='hnuser'>robtherobber</a> <span class='age' title='2024-08-30T14:00:41'><a href='item?id=41400807'>34 minutes ago</a></span> <span id='unv_41400807'></span> | <a href='hide?id=41400807&amp;goto=news'>hide</a> | <a href='item?id=41400807'>2&nbsp;comments</a></span></td>\
 \            </tr>\
 \            <tr class='spacer' style='height:5px'>\
 \              <td></td>\
 \            </tr>\
 \            <tr class='athing' id='41399047'>\
 \              <td align='right' valign='top' class='title'><span class='rank'>10.</span></td>\
 \              <td valign='top' class='votelinks'>\
 \                <center>\
 \                  <a id='up_41399047' href='vote?id=41399047&amp;how=up&amp;goto=news'>\
 \                  <div class='votearrow' title='upvote'></div></a>\
 \                </center>\
 \              </td>\
 \              <td class='title'><span class='titleline'><a href='https://rulebasedintegration.org/'>Rubi: Symbolic integrator based on an extensive system of integration rules</a> <span class='sitebit comhead'>(<a href='from?site=rulebasedintegration.org'><span class='sitestr'>rulebasedintegration.org</span></a>)</span></span></td>\
 \            </tr>\
 \            <tr>\
 \              <td colspan='2'></td>\
 \              <td class='subtext'><span class='subline'><span class='score' id='score_41399047'>29 points</span> by <a href='user?id=ducktective' class='hnuser'>ducktective</a> <span class='age' title='2024-08-30T09:25:13'><a href='item?id=41399047'>5 hours ago</a></span> <span id='unv_41399047'></span> | <a href='hide?id=41399047&amp;goto=news'>hide</a> | <a href='item?id=41399047'>17&nbsp;comments</a></span></td>\
 \            </tr>\
 \            <tr class='spacer' style='height:5px'>\
 \              <td></td>\
 \            </tr>\
 \            <tr class='athing' id='41398780'>\
 \              <td align='right' valign='top' class='title'><span class='rank'>11.</span></td>\
 \              <td valign='top' class='votelinks'>\
 \                <center>\
 \                  <a id='up_41398780' href='vote?id=41398780&amp;how=up&amp;goto=news'>\
 \                  <div class='votearrow' title='upvote'></div></a>\
 \                </center>\
 \              </td>\
 \              <td class='title'><span class='titleline'><a href='https://github.com/duneroadrunner/scpptool'>Scpptool – a tool to enforce a memory and data race safe subset of C++</a> <span class='sitebit comhead'>(<a href='from?site=github.com/duneroadrunner'><span class='sitestr'>github.com/duneroadrunner</span></a>)</span></span></td>\
 \            </tr>\
 \            <tr>\
 \              <td colspan='2'></td>\
 \              <td class='subtext'><span class='subline'><span class='score' id='score_41398780'>23 points</span> by <a href='user?id=coffeeaddict1' class='hnuser'>coffeeaddict1</a> <span class='age' title='2024-08-30T08:29:04'><a href='item?id=41398780'>6 hours ago</a></span> <span id='unv_41398780'></span> | <a href='hide?id=41398780&amp;goto=news'>hide</a> | <a href='item?id=41398780'>5&nbsp;comments</a></span></td>\
 \            </tr>\
 \            <tr class='spacer' style='height:5px'>\
 \              <td></td>\
 \            </tr>\
 \            <tr class='athing' id='41396260'>\
 \              <td align='right' valign='top' class='title'><span class='rank'>12.</span></td>\
 \              <td valign='top' class='votelinks'>\
 \                <center>\
 \                  <a id='up_41396260' href='vote?id=41396260&amp;how=up&amp;goto=news'>\
 \                  <div class='votearrow' title='upvote'></div></a>\
 \                </center>\
 \              </td>\
 \              <td class='title'><span class='titleline'><a href='https://github.com/libsdl-org/SDL/pull/9312'>SDL3 new GPU API merged</a> <span class='sitebit comhead'>(<a href='from?site=github.com/libsdl-org'><span class='sitestr'>github.com/libsdl-org</span></a>)</span></span></td>\
 \            </tr>\
 \            <tr>\
 \              <td colspan='2'></td>\
 \              <td class='subtext'><span class='subline'><span class='score' id='score_41396260'>225 points</span> by <a href='user?id=caspar' class='hnuser'>caspar</a> <span class='age' title='2024-08-29T23:04:32'><a href='item?id=41396260'>15 hours ago</a></span> <span id='unv_41396260'></span> | <a href='hide?id=41396260&amp;goto=news'>hide</a> | <a href='item?id=41396260'>82&nbsp;comments</a></span></td>\
 \            </tr>\
 \            <tr class='spacer' style='height:5px'>\
 \              <td></td>\
 \            </tr>\
 \            <tr class='athing' id='41397498'>\
 \              <td align='right' valign='top' class='title'><span class='rank'>13.</span></td>\
 \              <td valign='top' class='votelinks'>\
 \                <center>\
 \                  <a id='up_41397498' href='vote?id=41397498&amp;how=up&amp;goto=news'>\
 \                  <div class='votearrow' title='upvote'></div></a>\
 \                </center>\
 \              </td>\
 \              <td class='title'><span class='titleline'><a href='https://github.com/algora-io/tv'>Open Source Twitch for Developers</a> <span class='sitebit comhead'>(<a href='from?site=github.com/algora-io'><span class='sitestr'>github.com/algora-io</span></a>)</span></span></td>\
 \            </tr>\
 \            <tr>\
 \              <td colspan='2'></td>\
 \              <td class='subtext'><span class='subline'><span class='score' id='score_41397498'>74 points</span> by <a href='user?id=selvan' class='hnuser'>selvan</a> <span class='age' title='2024-08-30T03:28:27'><a href='item?id=41397498'>11 hours ago</a></span> <span id='unv_41397498'></span> | <a href='hide?id=41397498&amp;goto=news'>hide</a> | <a href='item?id=41397498'>18&nbsp;comments</a></span></td>\
 \            </tr>\
 \            <tr class='spacer' style='height:5px'>\
 \              <td></td>\
 \            </tr>\
 \            <tr class='athing' id='41394797'>\
 \              <td align='right' valign='top' class='title'><span class='rank'>14.</span></td>\
 \              <td valign='top' class='votelinks'>\
 \                <center>\
 \                  <a id='up_41394797' href='vote?id=41394797&amp;how=up&amp;goto=news'>\
 \                  <div class='votearrow' title='upvote'></div></a>\
 \                </center>\
 \              </td>\
 \              <td class='title'><span class='titleline'><a href='https://www.elastic.co/blog/elasticsearch-is-open-source-again'>Elasticsearch is open source, again</a> <span class='sitebit comhead'>(<a href='from?site=elastic.co'><span class='sitestr'>elastic.co</span></a>)</span></span></td>\
 \            </tr>\
 \            <tr>\
 \              <td colspan='2'></td>\
 \              <td class='subtext'><span class='subline'><span class='score' id='score_41394797'>653 points</span> by <a href='user?id=dakrone' class='hnuser'>dakrone</a> <span class='age' title='2024-08-29T20:10:02'><a href='item?id=41394797'>18 hours ago</a></span> <span id='unv_41394797'></span> | <a href='hide?id=41394797&amp;goto=news'>hide</a> | <a href='item?id=41394797'>356&nbsp;comments</a></span></td>\
 \            </tr>\
 \            <tr class='spacer' style='height:5px'>\
 \              <td></td>\
 \            </tr>\
 \            <tr class='athing' id='41391412'>\
 \              <td align='right' valign='top' class='title'><span class='rank'>15.</span></td>\
 \              <td valign='top' class='votelinks'>\
 \                <center>\
 \                  <a id='up_41391412' href='vote?id=41391412&amp;how=up&amp;goto=news'>\
 \                  <div class='votearrow' title='upvote'></div></a>\
 \                </center>\
 \              </td>\
 \              <td class='title'><span class='titleline'><a href='https://brave.com/blog/related-website-sets/'>Chrome is entrenching third-party cookies that will mislead users</a> <span class='sitebit comhead'>(<a href='from?site=brave.com'><span class='sitestr'>brave.com</span></a>)</span></span></td>\
 \            </tr>\
 \            <tr>\
 \              <td colspan='2'></td>\
 \              <td class='subtext'><span class='subline'><span class='score' id='score_41391412'>434 points</span> by <a href='user?id=NayamAmarshe' class='hnuser'>NayamAmarshe</a> <span class='age' title='2024-08-29T14:53:15'><a href='item?id=41391412'>23 hours ago</a></span> <span id='unv_41391412'></span> | <a href='hide?id=41391412&amp;goto=news'>hide</a> | <a href='item?id=41391412'>230&nbsp;comments</a></span></td>\
 \            </tr>\
 \            <tr class='spacer' style='height:5px'>\
 \              <td></td>\
 \            </tr>\
 \            <tr class='athing' id='41400327'>\
 \              <td align='right' valign='top' class='title'><span class='rank'>16.</span></td>\
 \              <td valign='top' class='votelinks'>\
 \                <center>\
 \                  <a id='up_41400327' href='vote?id=41400327&amp;how=up&amp;goto=news'>\
 \                  <div class='votearrow' title='upvote'></div></a>\
 \                </center>\
 \              </td>\
 \              <td class='title'><span class='titleline'><a href='https://www.wsj.com/health/healthcare/cancer-study-retracted-research-fallout-9573f842'>The Far-Reaching Ripple Effects of a Discredited Cancer Study</a> <span class='sitebit comhead'>(<a href='from?site=wsj.com'><span class='sitestr'>wsj.com</span></a>)</span></span></td>\
 \            </tr>\
 \            <tr>\
 \              <td colspan='2'></td>\
 \              <td class='subtext'><span class='subline'><span class='score' id='score_41400327'>15 points</span> by <a href='user?id=voisin' class='hnuser'>voisin</a> <span class='age' title='2024-08-30T13:08:02'><a href='item?id=41400327'>1 hour ago</a></span> <span id='unv_41400327'></span> | <a href='hide?id=41400327&amp;goto=news'>hide</a> | <a href='item?id=41400327'>4&nbsp;comments</a></span></td>\
 \            </tr>\
 \            <tr class='spacer' style='height:5px'>\
 \              <td></td>\
 \            </tr>\
 \            <tr class='athing' id='41358389'>\
 \              <td align='right' valign='top' class='title'><span class='rank'>17.</span></td>\
 \              <td valign='top' class='votelinks'>\
 \                <center>\
 \                  <a id='up_41358389' href='vote?id=41358389&amp;how=up&amp;goto=news'>\
 \                  <div class='votearrow' title='upvote'></div></a>\
 \                </center>\
 \              </td>\
 \              <td class='title'><span class='titleline'><a href='https://daily.jstor.org/the-bug-in-the-computer-bug-story/' rel='nofollow'>The Bug in the Computer Bug Story</a> <span class='sitebit comhead'>(<a href='from?site=jstor.org'><span class='sitestr'>jstor.org</span></a>)</span></span></td>\
 \            </tr>\
 \            <tr>\
 \              <td colspan='2'></td>\
 \              <td class='subtext'><span class='subline'><span class='score' id='score_41358389'>3 points</span> by <a href='user?id=refibrillator' class='hnuser'>refibrillator</a> <span class='age' title='2024-08-26T15:50:39'><a href='item?id=41358389'>1 hour ago</a></span> <span id='unv_41358389'></span> | <a href='hide?id=41358389&amp;goto=news'>hide</a> | <a href='item?id=41358389'>discuss</a></span></td>\
 \            </tr>\
 \            <tr class='spacer' style='height:5px'>\
 \              <td></td>\
 \            </tr>\
 \            <tr class='athing' id='41396501'>\
 \              <td align='right' valign='top' class='title'><span class='rank'>18.</span></td>\
 \              <td valign='top' class='votelinks'>\
 \                <center>\
 \                  <a id='up_41396501' href='vote?id=41396501&amp;how=up&amp;goto=news'>\
 \                  <div class='votearrow' title='upvote'></div></a>\
 \                </center>\
 \              </td>\
 \              <td class='title'><span class='titleline'><a href='https://bletchleypark.org.uk/'>Visit Bletchley Park</a> <span class='sitebit comhead'>(<a href='from?site=bletchleypark.org.uk'><span class='sitestr'>bletchleypark.org.uk</span></a>)</span></span></td>\
 \            </tr>\
 \            <tr>\
 \              <td colspan='2'></td>\
 \              <td class='subtext'><span class='subline'><span class='score' id='score_41396501'>127 points</span> by <a href='user?id=bookofjoe' class='hnuser'>bookofjoe</a> <span class='age' title='2024-08-29T23:46:25'><a href='item?id=41396501'>14 hours ago</a></span> <span id='unv_41396501'></span> | <a href='hide?id=41396501&amp;goto=news'>hide</a> | <a href='item?id=41396501'>53&nbsp;comments</a></span></td>\
 \            </tr>\
 \            <tr class='spacer' style='height:5px'>\
 \              <td></td>\
 \            </tr>\
 \            <tr class='athing' id='41390884'>\
 \              <td align='right' valign='top' class='title'><span class='rank'>19.</span></td>\
 \              <td valign='top' class='votelinks'>\
 \                <center>\
 \                  <a id='up_41390884' href='vote?id=41390884&amp;how=up&amp;goto=news'>\
 \                  <div class='votearrow' title='upvote'></div></a>\
 \                </center>\
 \              </td>\
 \              <td class='title'><span class='titleline'><a href='https://unchartedterritories.tomaspueyo.com/p/can-solar-costs-keep-shrinking'>Can solar costs keep shrinking?</a> <span class='sitebit comhead'>(<a href='from?site=tomaspueyo.com'><span class='sitestr'>tomaspueyo.com</span></a>)</span></span></td>\
 \            </tr>\
 \            <tr>\
 \              <td colspan='2'></td>\
 \              <td class='subtext'><span class='subline'><span class='score' id='score_41390884'>337 points</span> by <a href='user?id=GoRudy' class='hnuser'>GoRudy</a> <span class='age' title='2024-08-29T13:57:51'><a href='item?id=41390884'>1 day ago</a></span> <span id='unv_41390884'></span> | <a href='hide?id=41390884&amp;goto=news'>hide</a> | <a href='item?id=41390884'>514&nbsp;comments</a></span></td>\
 \            </tr>\
 \            <tr class='spacer' style='height:5px'>\
 \              <td></td>\
 \            </tr>\
 \            <tr class='athing' id='41397873'>\
 \              <td align='right' valign='top' class='title'><span class='rank'>20.</span></td>\
 \              <td valign='top' class='votelinks'>\
 \                <center>\
 \                  <a id='up_41397873' href='vote?id=41397873&amp;how=up&amp;goto=news'>\
 \                  <div class='votearrow' title='upvote'></div></a>\
 \                </center>\
 \              </td>\
 \              <td class='title'><span class='titleline'><a href='https://dl.acm.org/doi/pdf/10.1145/63039.63045'>Calendar Queues: A Fast O(1) Priority Queue Implementation (1988)</a> <span class='sitebit comhead'>(<a href='from?site=acm.org'><span class='sitestr'>acm.org</span></a>)</span></span></td>\
 \            </tr>\
 \            <tr>\
 \              <td colspan='2'></td>\
 \              <td class='subtext'><span class='subline'><span class='score' id='score_41397873'>43 points</span> by <a href='user?id=tithe' class='hnuser'>tithe</a> <span class='age' title='2024-08-30T05:07:15'><a href='item?id=41397873'>9 hours ago</a></span> <span id='unv_41397873'></span> | <a href='hide?id=41397873&amp;goto=news'>hide</a> | <a href='item?id=41397873'>19&nbsp;comments</a></span></td>\
 \            </tr>\
 \            <tr class='spacer' style='height:5px'>\
 \              <td></td>\
 \            </tr>\
 \            <tr class='athing' id='41393458'>\
 \              <td align='right' valign='top' class='title'><span class='rank'>21.</span></td>\
 \              <td valign='top' class='votelinks'>\
 \                <center>\
 \                  <a id='up_41393458' href='vote?id=41393458&amp;how=up&amp;goto=news'>\
 \                  <div class='votearrow' title='upvote'></div></a>\
 \                </center>\
 \              </td>\
 \              <td class='title'><span class='titleline'><a href='item?id=41393458'>Launch HN: CodeViz (YC S24) – Visual maps of your codebase in VS Code</a></span></td>\
 \            </tr>\
 \            <tr>\
 \              <td colspan='2'></td>\
 \              <td class='subtext'><span class='subline'><span class='score' id='score_41393458'>162 points</span> by <a href='user?id=LiamPrevelige' class='hnuser'>LiamPrevelige</a> <span class='age' title='2024-08-29T17:50:23'><a href='item?id=41393458'>20 hours ago</a></span> <span id='unv_41393458'></span> | <a href='hide?id=41393458&amp;goto=news'>hide</a> | <a href='item?id=41393458'>69&nbsp;comments</a></span></td>\
 \            </tr>\
 \            <tr class='spacer' style='height:5px'>\
 \              <td></td>\
 \            </tr>\
 \            <tr class='athing' id='41395925'>\
 \              <td align='right' valign='top' class='title'><span class='rank'>22.</span></td>\
 \              <td valign='top' class='votelinks'>\
 \                <center>\
 \                  <a id='up_41395925' href='vote?id=41395925&amp;how=up&amp;goto=news'>\
 \                  <div class='votearrow' title='upvote'></div></a>\
 \                </center>\
 \              </td>\
 \              <td class='title'><span class='titleline'><a href='https://github.com/google/closure-library'>Google Closure Library has been archived</a> <span class='sitebit comhead'>(<a href='from?site=github.com/google'><span class='sitestr'>github.com/google</span></a>)</span></span></td>\
 \            </tr>\
 \            <tr>\
 \              <td colspan='2'></td>\
 \              <td class='subtext'><span class='subline'><span class='score' id='score_41395925'>144 points</span> by <a href='user?id=benatkin' class='hnuser'>benatkin</a> <span class='age' title='2024-08-29T22:22:12'><a href='item?id=41395925'>16 hours ago</a></span> <span id='unv_41395925'></span> | <a href='hide?id=41395925&amp;goto=news'>hide</a> | <a href='item?id=41395925'>54&nbsp;comments</a></span></td>\
 \            </tr>\
 \            <tr class='spacer' style='height:5px'>\
 \              <td></td>\
 \            </tr>\
 \            <tr class='athing' id='41368239'>\
 \              <td align='right' valign='top' class='title'><span class='rank'>23.</span></td>\
 \              <td valign='top' class='votelinks'>\
 \                <center>\
 \                  <a id='up_41368239' href='vote?id=41368239&amp;how=up&amp;goto=news'>\
 \                  <div class='votearrow' title='upvote'></div></a>\
 \                </center>\
 \              </td>\
 \              <td class='title'><span class='titleline'><a href='https://bpfquery.com/?showing=kprobe'>Show HN: bpfquery – experimenting with compiling SQL to bpf(trace)</a> <span class='sitebit comhead'>(<a href='from?site=bpfquery.com'><span class='sitestr'>bpfquery.com</span></a>)</span></span></td>\
 \            </tr>\
 \            <tr>\
 \              <td colspan='2'></td>\
 \              <td class='subtext'><span class='subline'><span class='score' id='score_41368239'>32 points</span> by <a href='user?id=knuckleheads' class='hnuser'>knuckleheads</a> <span class='age' title='2024-08-27T14:55:09'><a href='item?id=41368239'>8 hours ago</a></span> <span id='unv_41368239'></span> | <a href='hide?id=41368239&amp;goto=news'>hide</a> | <a href='item?id=41368239'>6&nbsp;comments</a></span></td>\
 \            </tr>\
 \            <tr class='spacer' style='height:5px'>\
 \              <td></td>\
 \            </tr>\
 \            <tr class='athing' id='41396206'>\
 \              <td align='right' valign='top' class='title'><span class='rank'>24.</span></td>\
 \              <td valign='top' class='votelinks'>\
 \                <center>\
 \                  <a id='up_41396206' href='vote?id=41396206&amp;how=up&amp;goto=news'>\
 \                  <div class='votearrow' title='upvote'></div></a>\
 \                </center>\
 \              </td>\
 \              <td class='title'><span class='titleline'><a href='https://www.haskellforall.com/2024/08/firewall-rules-not-as-secure-as-you.html'>Firewall rules: not as secure as you think</a> <span class='sitebit comhead'>(<a href='from?site=haskellforall.com'><span class='sitestr'>haskellforall.com</span></a>)</span></span></td>\
 \            </tr>\
 \            <tr>\
 \              <td colspan='2'></td>\
 \              <td class='subtext'><span class='subline'><span class='score' id='score_41396206'>140 points</span> by <a href='user?id=jnord' class='hnuser'>jnord</a> <span class='age' title='2024-08-29T22:56:27'><a href='item?id=41396206'>15 hours ago</a></span> <span id='unv_41396206'></span> | <a href='hide?id=41396206&amp;goto=news'>hide</a> | <a href='item?id=41396206'>58&nbsp;comments</a></span></td>\
 \            </tr>\
 \            <tr class='spacer' style='height:5px'>\
 \              <td></td>\
 \            </tr>\
 \            <tr class='athing' id='41356279'>\
 \              <td align='right' valign='top' class='title'><span class='rank'>25.</span></td>\
 \              <td valign='top' class='votelinks'>\
 \                <center>\
 \                  <a id='up_41356279' href='vote?id=41356279&amp;how=up&amp;goto=news'>\
 \                  <div class='votearrow' title='upvote'></div></a>\
 \                </center>\
 \              </td>\
 \              <td class='title'><span class='titleline'><a href='https://blog.colinbreck.com/predicting-the-future-of-distributed-systems/'>Predicting the future of distributed systems</a> <span class='sitebit comhead'>(<a href='from?site=colinbreck.com'><span class='sitestr'>colinbreck.com</span></a>)</span></span></td>\
 \            </tr>\
 \            <tr>\
 \              <td colspan='2'></td>\
 \              <td class='subtext'><span class='subline'><span class='score' id='score_41356279'>53 points</span> by <a href='user?id=lutzh' class='hnuser'>lutzh</a> <span class='age' title='2024-08-26T11:56:19'><a href='item?id=41356279'>11 hours ago</a></span> <span id='unv_41356279'></span> | <a href='hide?id=41356279&amp;goto=news'>hide</a> | <a href='item?id=41356279'>10&nbsp;comments</a></span></td>\
 \            </tr>\
 \            <tr class='spacer' style='height:5px'>\
 \              <td></td>\
 \            </tr>\
 \            <tr class='athing' id='41364715'>\
 \              <td align='right' valign='top' class='title'><span class='rank'>26.</span></td>\
 \              <td valign='top' class='votelinks'>\
 \                <center>\
 \                  <a id='up_41364715' href='vote?id=41364715&amp;how=up&amp;goto=news'>\
 \                  <div class='votearrow' title='upvote'></div></a>\
 \                </center>\
 \              </td>\
 \              <td class='title'><span class='titleline'><a href='https://harpers.org/archive/2024/09/the-thin-purple-line-jasper-craven-private-security-guard/'>The Thin Purple Line</a> <span class='sitebit comhead'>(<a href='from?site=harpers.org'><span class='sitestr'>harpers.org</span></a>)</span></span></td>\
 \            </tr>\
 \            <tr>\
 \              <td colspan='2'></td>\
 \              <td class='subtext'><span class='subline'><span class='score' id='score_41364715'>57 points</span> by <a href='user?id=walterbell' class='hnuser'>walterbell</a> <span class='age' title='2024-08-27T05:09:24'><a href='item?id=41364715'>9 hours ago</a></span> <span id='unv_41364715'></span> | <a href='hide?id=41364715&amp;goto=news'>hide</a> | <a href='item?id=41364715'>34&nbsp;comments</a></span></td>\
 \            </tr>\
 \            <tr class='spacer' style='height:5px'>\
 \              <td></td>\
 \            </tr>\
 \            <tr class='athing' id='41400500'>\
 \              <td align='right' valign='top' class='title'><span class='rank'>27.</span></td>\
 \              <td valign='top' class='votelinks'>\
 \                <center>\
 \                  <a id='up_41400500' href='vote?id=41400500&amp;how=up&amp;goto=news'>\
 \                  <div class='votearrow' title='upvote'></div></a>\
 \                </center>\
 \              </td>\
 \              <td class='title'><span class='titleline'><a href='https://highscalability.com/behind-aws-s3s-massive-scale/' rel='nofollow'>Behind AWS S3's Scale</a> <span class='sitebit comhead'>(<a href='from?site=highscalability.com'><span class='sitestr'>highscalability.com</span></a>)</span></span></td>\
 \            </tr>\
 \            <tr>\
 \              <td colspan='2'></td>\
 \              <td class='subtext'><span class='subline'><span class='score' id='score_41400500'>8 points</span> by <a href='user?id=zerojames' class='hnuser'>zerojames</a> <span class='age' title='2024-08-30T13:25:48'><a href='item?id=41400500'>1 hour ago</a></span> <span id='unv_41400500'></span> | <a href='hide?id=41400500&amp;goto=news'>hide</a> | <a href='item?id=41400500'>4&nbsp;comments</a></span></td>\
 \            </tr>\
 \            <tr class='spacer' style='height:5px'>\
 \              <td></td>\
 \            </tr>\
 \            <tr class='athing' id='41400663'>\
 \              <td align='right' valign='top' class='title'><span class='rank'>28.</span></td>\
 \              <td valign='top' class='votelinks'>\
 \                <center>\
 \                  <a id='up_41400663' href='vote?id=41400663&amp;how=up&amp;goto=news'>\
 \                  <div class='votearrow' title='upvote'></div></a>\
 \                </center>\
 \              </td>\
 \              <td class='title'><span class='titleline'><a href='https://www.aibase.com/news/11434'>Suddenly Runway deleted all content on HuggingFace and GitHub</a> <span class='sitebit comhead'>(<a href='from?site=aibase.com'><span class='sitestr'>aibase.com</span></a>)</span></span></td>\
 \            </tr>\
 \            <tr>\
 \              <td colspan='2'></td>\
 \              <td class='subtext'><span class='subline'><span class='score' id='score_41400663'>15 points</span> by <a href='user?id=console-log' class='hnuser'>console-log</a> <span class='age' title='2024-08-30T13:44:50'><a href='item?id=41400663'>50 minutes ago</a></span> <span id='unv_41400663'></span> | <a href='hide?id=41400663&amp;goto=news'>hide</a> | <a href='item?id=41400663'>discuss</a></span></td>\
 \            </tr>\
 \            <tr class='spacer' style='height:5px'>\
 \              <td></td>\
 \            </tr>\
 \            <tr class='athing' id='41395226'>\
 \              <td align='right' valign='top' class='title'><span class='rank'>29.</span></td>\
 \              <td><img src='s.gif' height='1' width='14'></td>\
 \              <td class='title'><span class='titleline'><a href='https://www.ycombinator.com/companies/poly/jobs/L4ObRgn-founding-platform-full-stack-front-end-engineer'>Poly (YC S22) is hiring Rust experts in SF to build 'Arc browser for files'</a> <span class='sitebit comhead'>(<a href='from?site=ycombinator.com'><span class='sitestr'>ycombinator.com</span></a>)</span></span></td>\
 \            </tr>\
 \            <tr>\
 \              <td colspan='2'></td>\
 \              <td class='subtext'>\
 \                <span class='age' title='2024-08-29T21:00:51'><a href='item?id=41395226'>17 hours ago</a></span> | <a href='hide?id=41395226&amp;goto=news'>hide</a>\
 \              </td>\
 \            </tr>\
 \            <tr class='spacer' style='height:5px'>\
 \              <td></td>\
 \            </tr>\
 \            <tr class='athing' id='41391822'>\
 \              <td align='right' valign='top' class='title'><span class='rank'>30.</span></td>\
 \              <td valign='top' class='votelinks'>\
 \                <center>\
 \                  <a id='up_41391822' href='vote?id=41391822&amp;how=up&amp;goto=news'>\
 \                  <div class='votearrow' title='upvote'></div></a>\
 \                </center>\
 \              </td>\
 \              <td class='title'><span class='titleline'><a href='https://github.com/rscott2049/pico-rmii-ethernet_nce'>Raspberry Pi Pico does line rate 100M Ethernet</a> <span class='sitebit comhead'>(<a href='from?site=github.com/rscott2049'><span class='sitestr'>github.com/rscott2049</span></a>)</span></span></td>\
 \            </tr>\
 \            <tr>\
 \              <td colspan='2'></td>\
 \              <td class='subtext'><span class='subline'><span class='score' id='score_41391822'>220 points</span> by <a href='user?id=rscott2049' class='hnuser'>rscott2049</a> <span class='age' title='2024-08-29T15:29:13'><a href='item?id=41391822'>23 hours ago</a></span> <span id='unv_41391822'></span> | <a href='hide?id=41391822&amp;goto=news'>hide</a> | <a href='item?id=41391822'>55&nbsp;comments</a></span></td>\
 \            </tr>\
 \            <tr class='spacer' style='height:5px'>\
 \              <td></td>\
 \            </tr>\
 \            <tr class='morespace' style='height:10px'>\
 \              <td></td>\
 \            </tr>\
 \            <tr>\
 \              <td colspan='2'></td>\
 \              <td class='title'>\
 \                <a href='?p=2' class='morelink' rel='next'>More</a>\
 \              </td>\
 \            </tr>\
 \          </table>\
 \        </td>\
 \      </tr>\
 \      <tr>\
 \        <td>\
 \          <img src='s.gif' height='10' width='0'>\
 \          <table width='100%' cellspacing='0' cellpadding='1'>\
 \            <tr>\
 \              <td bgcolor='#FF6600'></td>\
 \            </tr>\
 \          </table><br>\
 \          <center>\
 \            <span class='yclinks'><a href='newsguidelines.html'>Guidelines</a> | <a href='newsfaq.html'>FAQ</a> | <a href='lists'>Lists</a> | <a href='https://github.com/HackerNews/API'>API</a> | <a href='security.html'>Security</a> | <a href='https://www.ycombinator.com/legal/'>Legal</a> | <a href='https://www.ycombinator.com/apply/'>Apply to YC</a> | <a href='mailto:hn@ycombinator.com'>Contact</a></span><br>\
 \            <br>\
 \            <form method='get' action='//hn.algolia.com/'>\
 \              Search: <input type='text' name='q' size='17' autocorrect='off' spellcheck='false' autocapitalize='off' autocomplete='off'>\
 \            </form>\
 \          </center>\
 \        </td>\
 \      </tr>\
 \    </table>\
 \  </center>\
 \  <script type='text/javascript' src='hn.js?2QLhhCRy3pNA8Rfan1sV'></script>\
 \</body>\
 \</html>"
