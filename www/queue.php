<?php

require_once(dirname(__FILE__) . '/library/api.php');

StartPage();

echo '<h2>処理キューの状況</h2>';
echo '<div class="container">';
echo "<table class=\"table table-striped table-bordered\" width=\"100%\">\n";
echo "<tr><td>処理待ちのバトル数</td><td>" .
     Database::SelectCell(
        'SELECT COUNT(*)
         FROM battle NATURAL JOIN (
            SELECT DISTINCT battle_id
            FROM punter WHERE punter_score IS NULL) AS punter
         WHERE battle_lock < NOW()') .
     " 件</td></tr>\n";
echo "<tr><td style=\"width: 20em;\">処理中のバトル数</td><td>" .
     Database::SelectCell(
        'SELECT COUNT(*) FROM battle WHERE battle_lock > NOW()') .
     " 件</td></tr>\n";
echo "<tr><td>直近 10 分に処理したバトル数</td><td>" .
     Database::SelectCell(
        'SELECT COUNT(*) FROM battle
        WHERE battle_modified > NOW() - INTERVAL 10 MINUTE') .
     " 件</td></tr>\n";
echo "<tr><td>直近 60 分に処理したバトル数</td><td>" .
     Database::SelectCell(
        'SELECT COUNT(*) FROM battle
        WHERE battle_modified > NOW() - INTERVAL 60 MINUTE') .
     " 件</td></tr>\n";
echo "<tr><td style=\"width: 20em;\">20 分以上処理中のバトル数</td><td>" .
     Database::SelectCell(
        'SELECT COUNT(*) FROM battle
         WHERE battle_lock > NOW() AND
               battle_created < NOW() - INTERVAL 20 MINUTE') .
     " 件</td></tr>\n";
echo "<tr><td style=\"width: 20em;\">60 分以上処理中のバトル数</td><td>" .
     Database::SelectCell(
        'SELECT COUNT(*) FROM battle
         WHERE battle_lock > NOW() AND
               battle_created < NOW() - INTERVAL 60 MINUTE') .
     " 件</td></tr>\n";
echo "<tr><td style=\"width: 20em;\">3 時間以上処理中のバトル数</td><td>" .
     Database::SelectCell(
        'SELECT COUNT(*) FROM battle
         WHERE battle_lock > NOW() AND
               battle_created < NOW() - INTERVAL 180 MINUTE') .
     " 件</td></tr>\n";
echo "</table>\n";
