<?php

require_once(dirname(__FILE__) . '/library/api.php');

StartPage();

echo '<h2>順位表</h2>';
echo '<div class="container">';

Database::Command('
    CREATE TEMPORARY TABLE candidate_battle
    SELECT battle_id
    FROM battle ORDER BY battle_created DESC LIMIT 5000');

Database::Command('
    CREATE TEMPORARY TABLE candidate_ai
    SELECT DISTINCT ai_id
    FROM candidate_battle NATURAL JOIN punter');

$maps = [];
foreach (Database::Select('SELECT * FROM map') as $map) {
  $maps[$map['map_id']] = $map;
}

$battles = [];
foreach (Database::Select(
    'SELECT * FROM candidate_battle NATURAL JOIN battle NATURAL JOIN map')
    as $battle) {
  $battles[$battle['battle_id']] = $battle;
}

foreach (Database::Select(
    'SELECT * FROM candidate_battle NATURAL JOIN punter
     WHERE punter_score IS NOT NULL ORDER BY punter_id') as $punter) {
  $battles[$punter['battle_id']]['punters'][] = $punter;
}

$ais = [];
foreach (Database::Select('
    SELECT * FROM candidate_ai NATURAL JOIN ai') as $ai) {
  $ais[$ai['ai_id']] = $ai;
}

$records = [];
$num_invalids = 0;
foreach ($battles as $battle) {
  if ($battle['map_capacity'] != @count($battle['punters'])) {
    $num_invalids++;
    continue;
  }
  $punters = [];
  foreach ($battle['punters'] as $punter_index => $punter) {
    $punter['punter_index'] = $punter_index;
    $punters[sprintf('%08d', $punter['punter_score']) .
             $punter_index] = $punter;
  }
  krsort($punters);
  foreach (array_values($punters) as $punter_rank => $punter) {
    $punter['punter_rank'] = $punter_rank;
    $records[$battle['map_id']][$punter['ai_id']][] = $punter;
  }
}

if ($num_invalids > 0) {
  echo '<div class="alert alert-warning" role="alert">対象となる ' .
       count($battles) . ' 件のバトルの内 ' . $num_invalids .
       ' 件が不完全なバトルとして除外されました。</div>';
}

$average_scores = [];
foreach ($records as $map_id => $map_records) {
  $map_capacity = intval($maps[$map_id]['map_capacity']);
  foreach ($map_records as $ai_id => $ai_records) {
    $scores = [];
    foreach ($ai_records as $record) {
      $scores[] = ($record['punter_index'] - ($map_capacity - 1) / 2) * 0.5 +
                  ($record['punter_rank'] - ($map_capacity - 1) / 2) * -1;
    }
    $score_info =
        ['rank_sum' => array_sum($scores), 'rank_count' => count($scores)];
    $score_info['rank_average'] =
        $score_info['rank_sum'] / $score_info['rank_count'];
    $average_scores[$map_id][$ai_id] = $score_info;
  }
}

$num_ranked_ais = [];
foreach ($average_scores as $map_id => $scores) {
  uasort($scores, function($lhs, $rhs) {
    if ($lhs['rank_average'] > $rhs['rank_average']) return -1;
    if ($lhs['rank_average'] < $rhs['rank_average']) return 1;
    return 0;
  });
  foreach ($scores as $ai_id => $score) {
    $ais[$ai_id]['maps'][$map_id] =
        $score + ['rank' => @++$num_ranked_ais[$map_id]];
  }
}

foreach ($ais as $ai_id => $ai) {
  $ai_score = 0;
  foreach ($maps as $map) {
    $ai_score += isset($ai['maps'][$map['map_id']]) ? $ai['maps'][$map['map_id']]['rank'] : count($ais);
  }
  $ais[$ai_id]['ai_score'] = $ai_score;
}
uasort($ais, function($lhs, $rhs) {
  if ($lhs['ai_score'] < $rhs['ai_score']) return -1;
  if ($lhs['ai_score'] > $rhs['ai_score']) return 1;
  return 0;
});

echo "<div style=\"overflow-x:scroll\"><table class=\"table table-striped table-bordered\" style=\"table-layout:fixed\">\n";
echo '<tr><th style="width: 12em">AI名</th>';
foreach ($maps as $map) {
  echo "<th style=\"width: 7em; word-wrap:break-word;\">{$map['map_key']}</th>";
}
echo "</tr>\n";

foreach (array_keys($ais) as $rank => $ai_id) {
  $ai = $ais[$ai_id];
  echo "<tr><td>" . ($rank + 1) . " 位 … {$ai['ai_key']}</td>";
  foreach ($maps as $map) {
    if (!isset($ai['maps'][$map['map_id']])) {
      echo '<td>データなし</td>';
      continue;
    }
    $rank = $ai['maps'][$map['map_id']];
    $background = 'inherit';
    switch ($rank['rank']) {
      case 1: $background = '#ffee55'; break;
      case 2: $background = '#cccccc'; break;
      case 3: $background = '#bb9955'; break;
      default: $background = 'inherit'; break;
    }
    echo "<td style=\"text-align:center; background: $background\">";
    echo "{$rank['rank']} 位 / {$rank['rank_count']} 回<br>";
    echo sprintf("%+.2f", $rank['rank_sum'] / $rank['rank_count']) . "</td>";
  }
  echo "</tr>\n";
}

echo "</table></div>\n";

echo '</div>';

