<?php

require_once(dirname(__FILE__) . '/library/api.php');

StartPage();

$battles = [];
foreach (Database::Select('
    SELECT * FROM battle NATURAL JOIN map
    ORDER BY battle_created DESC LIMIT 100') as $battle) {
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
echo '<div class="container">';

foreach ($battles as $battle) {
  echo "<h3>バトル {$battle['battle_id']}</h3>\n";
  echo "<table class=\"table table-striped table-bordered\" width=\"100%\">\n";
  echo '<tr><td width="20%">バトルID</td><td>' . $battle['battle_id'] . "</td></tr>\n";
  echo '<tr><td>マップ</td><td>' . $battle['map_key'] . "</td></tr>\n";
  echo '<tr><td>作成時刻</td><td>' . $battle['battle_created'] . "</td></tr>\n";
  echo '<tr><td>結果</td><td>';
  if (!isset($battle['punters'])) {
    echo 'Punter が存在しません';
  } else {
    echo '<ul style="margin: 0">';
    foreach ($battle['punters'] as $punter) {
      echo "<li>" . $punter['ai_key'] . ' … ' . $punter['punter_score'] . ' 点</li>';
    }
    echo '</ul></td></tr>';
  }
  echo "</table>\n";
  // print_r($battle);
}

echo '</div>';
