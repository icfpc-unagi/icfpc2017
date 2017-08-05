<?php

function KeepAlive($interval = 10) {
  global $battle;
  $result = file_get_contents(
      "http://proxy.sx9.jp/api/keep_alive_battle.php?battle_id={$battle['battle_id']}&interval=$interval");
}

$battle = json_decode(file_get_contents(
    'http://proxy.sx9.jp/api/get_battle.php'), TRUE);
if (!isset($battle['battle_id'])) {
  fwrite(STDERR, 'No battle.');
  exit();
}
KeepAlive();

$args = [];
foreach ($battle['punter'] as $punter) {
  $args[$punter['punter_id']] = $punter['ai_command'];
}

$master = [
    '/binary/local',
    "--map=/github/map/{$battle['map_key']}.json"];
foreach ($battle['punter'] as $punter) {
  $master[] = $punter['ai_command'];
}

$ninestream = [
    '/binary/ninestream',
    '--communicate',
    '--master=' . implode(' ', array_map('escapeshellarg', $master))];


function GetScores($command) {
  fwrite(STDERR, "Running command: $command\n");
  exec($command, $output, $return);
  foreach ($output as $line) {
    $result = json_decode($line, TRUE);
    if (isset($result['scores'])) {
      return $result;
    }
  }
  fwrite(STDERR, 'Score is not found.');
  exit(1);
}

$scores = GetScores(implode(' ', array_map('escapeshellarg', $ninestream)));
foreach ($scores['scores'] as $index => $score) {
  $punter = $battle['punter'][$index];
  $result = file_get_contents(
      "http://proxy.sx9.jp/api/update_punter.php?" .
      "punter_id={$punter['punter_id']}&punter_score={$score['score']}");
}
