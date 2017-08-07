<?php

require_once(dirname(__FILE__) . '/library/api.php');

StartPage();

$is_queue = @intval($_GET['is_queue']);
$map_id = @intval($_GET['map_id']);

$where = 'WHERE TRUE';
if ($map_id > 0) {
  $where .= ' AND map_id = ' . $map_id;
}

if (!$is_queue) {
  $sql = "SELECT battle.*, map.*
          FROM battle NATURAL JOIN map NATURAL JOIN punter
          $where
          GROUP BY battle_id
          HAVING SUM(CASE WHEN punter_score IS NULL THEN 1 ELSE 0 END) = 0
          ORDER BY battle_modified DESC LIMIT 100";
} else {
  $sql = "SELECT * FROM battle NATURAL JOIN map
          $where
          ORDER BY battle_created DESC LIMIT 100";
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
echo '<form action="?" method="GET">';
echo '表示順序 <select class="form-control" name="is_queue">';
echo '<option value=""' . (!$is_queue ? ' selected' : '') . '>処理順</option>';
echo '<option value="1"' . ($is_queue ? ' selected' : '') . '>追加順</option>';
echo '</select>';

echo 'マップの種類 <select class="form-control" name="map_id">';
echo '<option value="0"' . ($map_id == 0 ? ' selected' : '') . '>すべて</option>';
foreach (Database::Select('SELECT * FROM map') as $map) {
  echo '<option value="' . $map['map_id'] . '"' . ($map_id == $map['map_id'] ? ' selected' : '') . '>' . $map['map_key'] . '</option>';
}
echo '</select><br>';

echo '<input class="form-control" type="submit" value="検索">';

echo '<div class="container">';

foreach ($battles as $battle) {
  ShowBattle($battle);
}

echo '</div>';
