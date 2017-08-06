<?php

require_once(dirname(__FILE__) . '/library/api.php');

StartPage();

$is_queue = isset($_GET['is_queue']);

if (!$is_queue) {
  $sql = 'SELECT battle.*, map.*
          FROM battle NATURAL JOIN map NATURAL JOIN punter GROUP BY battle_id
          HAVING SUM(CASE WHEN punter_score IS NULL THEN 1 ELSE 0 END) = 0
          ORDER BY battle_modified DESC LIMIT 100';
} else {
  $sql = 'SELECT * FROM battle NATURAL JOIN map
          ORDER BY battle_created DESC LIMIT 100';
}

$battles = [];
foreach (Database::Select($sql) as $battle) {
  $battles[$battle['battle_id']] = $battle;
}

if (count($battles) == 0) {
  echo 'No battles.';
  exit();
}

$punters = Database::Select('
    SELECT * FROM punter NATURAL JOIN ai
    WHERE battle_id IN (' . implode(', ', array_keys($battles)) . ')');
foreach ($punters as $punter) {
  $battles[$punter['battle_id']]['punters'][] = $punter;
}

echo '<h2>最新バトル一覧</h2>';
echo '<ul class="nav nav-tabs">';
echo '<li role="presentation"' . (!$is_queue ? ' class="active"' : '') . "><a href=\"?\">処理順</a></li>\n";
echo '<li role="presentation"' . ($is_queue ? ' class="active"' : '') . "><a href=\"?is_queue=1\">追加順</a></li>\n";
echo "</ul>\n";
echo '<div class="container">';

foreach ($battles as $battle) {
  ShowBattle($battle);
}

echo '</div>';
