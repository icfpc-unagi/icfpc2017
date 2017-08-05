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
    "--map=/github/map/{$battle['battle_map']}.json"];
foreach ($battle['punter'] as $punter) {
  $master[] = $punter['ai_command'];
}

$ninestream = [
    '/binary/ninestream',
    '--communicate',
    '--master=' . implode(' ', array_map('escapeshellcmd', $master))];

exec(implode(' ', array_map('escapeshellcmd', $ninestream)), $output, $return);

print_r($output);
