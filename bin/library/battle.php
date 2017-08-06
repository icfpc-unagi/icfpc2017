<?php

$keep_alive = 0;

function KeepAlive($interval = 10) {
  global $battle, $keep_alive;
  if (time() + $interval * 0.5 < $keep_alive) {
    return;
  }
  $keep_alive = time() + $interval;

  $result = file_get_contents(
      "http://proxy.sx9.jp/api/keep_alive_battle.php?battle_id={$battle['battle_id']}&interval=$interval");
}

$battle = json_decode(file_get_contents(
    'http://proxy.sx9.jp/api/get_battle.php'), TRUE);
if (!isset($battle['battle_id'])) {
  fwrite(STDERR, "No battle.\n");
  file_get_contents('http://proxy.sx9.jp/api/add_random_battle.php');
  exit();
}

$args = [];
foreach ($battle['punter'] as $punter) {
  $args[$punter['punter_id']] = $punter['ai_command'];
}

$master = [
    '/binary/local',
    '--timeout=2000',
    "--map=/github/map/{$battle['map_key']}.json"];
foreach ($battle['punter'] as $punter) {
  $master[] = $punter['ai_command'];
}

$ninestream = [
    '/binary/ninestream',
    '--communicate',
    '--master=' . implode(' ', array_map('escapeshellarg', $master))];


function Post($url, $data) {
  $data = http_build_query($data);

  $context_options = [
      'http' => [
          'method' => 'POST',
          'header'=>
              "Content-Type: application/x-www-form-urlencoded\r\n" .
              "Content-Length: " . strlen($data) . "\r\n",
          'content' => $data]];

  $context = stream_context_create($context_options);
  $result = file_get_contents($url, false, $context);
}

function GetScores($command) {
  global $battle;
  fwrite(STDERR, "Running command: $command\n");
  KeepAlive(60);
  $handle = popen($command, 'r');
  $output = '';
  while (!feof($handle) && ($buffer = fread($handle, 1024)) !== FALSE) {
    $output .= $buffer;
    KeepAlive(60);
  }
  pclose($handle);
  Post("http://proxy.sx9.jp/api/add_battle_log.php?" .
       "battle_id={$battle['battle_id']}",
       ['battle_log_data' => $output]);
  foreach (explode("\n", $output) as $line) {
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
