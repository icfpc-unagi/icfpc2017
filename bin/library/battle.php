<?php

$keep_alive = 0;

function KeepAlive($interval = 10) {
  global $battle, $keep_alive;
  if (time() + $interval * 0.3 < $keep_alive) {
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
fwrite(STDERR, "Battle ID: {$battle['battle_id']}\n");

$args = [];
foreach ($battle['punter'] as $punter) {
  $args[$punter['punter_id']] = $punter['ai_command'];
}

$master = [
    '/binary/local',
    '--timeout=5000',
    '--logtostderr',
    "--map=/github/map/{$battle['map_key']}.json"];
foreach ($battle['punter'] as $punter) {
  $master[] = $punter['ai_command'] . ' 2>/dev/null';
}

$stderr_file = '/tmp/' . md5(microtime() . rand());

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
  global $battle, $stderr_file;
  fwrite(STDERR, "Running command: $command\n");
  KeepAlive(120);
  $handle = popen($command, 'r');
  $output = '';
  while (!feof($handle) && ($buffer = fread($handle, 1024)) !== FALSE) {
    $output .= $buffer;
    echo $buffer;
    KeepAlive(120);
  }
  pclose($handle);
  Post("http://proxy.sx9.jp/api/add_battle_log.php?" .
       "battle_id={$battle['battle_id']}",
       ['battle_log_data' => $output,
        'battle_log_info' => gethostname() . "\nCommand: " . $command . "\n\n" . file_get_contents($stderr_file)]);
  @unlink($stderr_file);
  $passed_punters = [];
  foreach (explode("\n", $output) as $line) {
    $result = json_decode($line, TRUE);
    if (isset($result['pass'])) {
      $passed_punters[$result['pass']['punter']] = TRUE;
    }
    if (isset($result['scores'])) {
      foreach ($result['scores'] as $index => $score) {
        $result['scores'][$index]['punter_pass'] = @intval($passed_punters[$score['punter']]);
      }
      return $result;
    }
  }
  fwrite(STDERR, 'Score is not found.');
  exit(1);
}

$scores = GetScores(implode(' ', array_map('escapeshellarg', $ninestream)) . ' 2>' . $stderr_file);
foreach ($scores['scores'] as $index => $score) {
  $punter = $battle['punter'][$index];
  $result = file_get_contents(
      "http://proxy.sx9.jp/api/update_punter.php?" .
      "punter_id={$punter['punter_id']}&punter_score={$score['score']}&punter_pass={$score['punter_pass']}");
}
