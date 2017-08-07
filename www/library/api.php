<?php

require_once(dirname(__FILE__) . '/config.php');
// define('DEBUG_MODE', 'sql');
define('API_HOST', 'http://db.sx9.jp');

$STYLESHEET = '';

function Fail($message, $status = '400 Bad Request') {
  header('HTTP/1.1 ' . $status);
  echo trim($message) . "\n";
  exit();
}

function Execute($params) {
  $data = http_build_query($params, "", "&");
  $context = stream_context_create([
      'http' => [
          'method' => 'POST',
          'header' =>
              "Content-Type: application/x-www-form-urlencoded\r\n" .
              "Content-Length: " . strlen($data),
          'content' => $data]]);
  $output = file_get_contents(API_HOST . '/exec.php', false, $context);
  if (strlen($output) == 0) {
    return NULL;
  }
  return json_decode($output, TRUE);
}

function GetParameter($name) {
  if (isset($_REQUEST[$name])) {
    return $_REQUEST[$name];
  } else if (isset($_FILES[$name])) {
    return file_get_contents($_FILES[$name]['tmp_name']);
  } else if (isset($_ENV[$name])) {
    return $_ENV[$name];
  } else if (isset($_ENV[$name . '_file'])) {
    return file_get_contents($_ENV[$name . '_file']);
  }
  return NULL;
}

function GetParameterOrDie($name) {
  $result = GetParameter($name);
  if (is_null($result)) {
    Fail("$name is required.");
  }
  return $result;
}

function GetToken() {
  for ($i = 0; $i < 10; $i++) {
    $current_time = intval(array_sum(array_map(
        'floatval', explode(' ', microtime()))) * 1000000);
    Database::Command('
        UPDATE `token` SET `token_value` = {value}
        WHERE `token_id` = "api" AND
              `token_value` < {value} - 1100000 LIMIT 1',
        ['value' => $current_time]);
    if (Database::AffectedRows() > 0) {
      return TRUE;
    }
    usleep(500 * 1000);
  }
  return FALSE;
}

function CallApi($path, $params = NULL) {
  $url = "http://2016sv.icfpcontest.org/api${path}";

  for ($i = 0; $i < 3; $i++) {
    if (!GetToken()) {
      sleep(rand(1, pow(2, $i)));
      continue;
    }
    fwrite(STDERR, "Calling $url...");
    $curl = curl_init($url);
    if (!is_null($params)) {
      curl_setopt($curl, CURLOPT_POST, TRUE);
      curl_setopt($curl, CURLOPT_POSTFIELDS, http_build_query($params));
    }
    curl_setopt($curl, CURLOPT_RETURNTRANSFER, TRUE);
    curl_setopt($curl, CURLOPT_FOLLOWLOCATION, TRUE);
    curl_setopt($curl, CURLOPT_ENCODING, '');
    curl_setopt($curl, CURLOPT_HTTPHEADER,
                ['X-API-Key: 42-59d13e150cd0de2292ef5adf36cb1e26',
                 'Expect: ']);
    $output = curl_exec($curl);
    $status = curl_getinfo($curl, CURLINFO_HTTP_CODE);
    curl_close($curl);
    Database::Command(
        'INSERT INTO `api`{api}',
        ['api' => [
            'api_path' => $path,
            'api_output' => $output,
            'api_status' => intval($status)]]);
    if ($status != 429) break;
    sleep(pow(2, $i));
  }
  if ($status != 200 && $status != 400 && $status != 403) {
    return NULL;
  }
  return $output;
}

function GetBlob($hash) {
  $data = Database::SelectCell(
      'SELECT `blob_data` FROM `blob` WHERE `blob_id` = {blob_id}',
      ['blob_id' => $hash]);
  if ($data !== FALSE) {
    return $data;
  }
  $data = CallApi("/blob/$hash");
  if (is_null($data)) {
    return NULL;
  }
  Database::Command(
      'INSERT `blob`(`blob_id`, `blob_data`) VALUES({blob_id}, {blob_data})',
      ['blob_id' => $hash, 'blob_data' => $data]);
  return $data;
}

function UpdateSnapshot() {
  $data = json_decode(CallApi('/snapshot/list'), TRUE);
  if ($data['ok'] !== TRUE) {
    return -1;
  }
  $values = [];
  foreach ($data['snapshots'] as $snapshot) {
    $values[] = [
        'snapshot_id' => $snapshot['snapshot_time'],
        'blob_id' => $snapshot['snapshot_hash']];
  }
  Database::Command('INSERT IGNORE INTO `snapshot` {values}',
                    ['values' => $values]);
  return Database::AffectedRows();
}

function GetSnapshot() {
  global $argv;

  $blob_id = NULL;
  if (isset($_GET['blob_id'])) {
    $blob_id = $_GET['blob_id'];
  } else if (isset($argv[1])) {
    $blob_id = $argv[1];
  } else {
    $blob_id = Database::SelectCell(
        'SELECT `blob_id` FROM `snapshot` ORDER BY `snapshot_id` DESC LIMIT 1');
  }
  $data = GetBlob($blob_id);
  if (is_null($data)) {
    return NULL;
  }
  $data = json_decode($data, TURE);
  if (!isset($data['problems'])) {
    return NULL;
  }
  return $data;
}

function SubmitSolution($problem_id, $data) {
  $data = CallApi('/solution/submit',
                  ['problem_id' => $problem_id, 'solution_spec' => $data]);
  if (is_null($data)) {
    return NULL;
  }
  $data = json_decode($data, TURE);
  if (!isset($data['ok'])) {
    return NULL;
  }
  return $data;
}

function SubmitProblem($publish_time, $data) {
  $data = CallApi('/problem/submit',
                  ['publish_time' => $publish_time, 'solution_spec' => $data]);
  if (is_null($data)) {
    return NULL;
  }
  $data = json_decode($data, TURE);
  if (!isset($data['ok'])) {
    return NULL;
  }
  return $data;
}

function FormatData($data) {
  $data = preg_replace('%\\s*[\\r\\n]+\\s*%', "\n", trim($data));
  return $data . "\n";
}

function RenderPage($buffer) {
  $output = '<!doctype html><html><head><meta charset="UTF-8">';
  $output .= '<title>ICFPC 2017</title>';
  $output .= '<link rel="stylesheet" href="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/css/bootstrap.min.css">';
  $output .= '<link rel="stylesheet" href="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap-theme.min.css" integrity="sha384-rHyoN1iRsVXV4nD0JutlnGaslCJuC7uwjduW9SVrLvRYooPp2bWYgmgJQIXwl/Sp" crossorigin="anonymous">';
  $output .= '<style>table.layout { width: 100%; table-layout: fixed } table.layout > tbody > tr > td { padding: 20px; vertical-align: top; } .pending { color: #aaa } </style>';
  $output .= "<style>{$GLOBALS['STYLESHEET']}</style>\n";
  $output .= '</head><body>';
  $output .= '<nav class="navbar navbar-default">
      <div class="container">
        <div class="navbar-header">
          <button type="button" class="navbar-toggle collapsed" data-toggle="collapse" data-target="#navbar-collapse" aria-expanded="false">
            <span class="sr-only">Toggle navigation</span>
            <span class="icon-bar"></span>
            <span class="icon-bar"></span>
            <span class="icon-bar"></span>
          </button>
          <a class="navbar-brand" href="/">🐍 Team Unagi</a>
        </div>
        <div class="collapse navbar-collapse" id="navbar-collapse">
          <ul class="nav navbar-nav">
            
            <li class="False">
              <a href="/">
                順位表
              </a>
            </li>
            <li class="False">
              <a href="/latest_battles.php">
                最新バトル一覧
              </a>
            </li>
            <li class="False">
              <a href="/queue.php">
                処理キュー状況
              </a>
            </li>
          </ul>
        </div>
      </div>
    </nav>';
  $output .= '<div class="container">';
  $output .= "\n$buffer\n";
  $output .= "</div></body></html>\n";
  return $output;
}

function StartPage() {
  ob_start('RenderPage');
}

function Color($id, $num) {
  $h = 6 * ($id + 1) / ($num + 1);
  $s = 1;
  $v = 170 / 255;
  $c = $v * $s;
  $x = $c * (1 - abs(fmod($h, 2) - 1));
  $r = $v - $c;
  $g = $v - $c;
  $b = $v - $c;
  if ($h < 1) {
    $r += $c; $g += $x;
  } else if ($h < 2) {
    $g += $c; $r += $x;
  } else if ($h < 3) {
    $g += $c; $b += $x;
  } else if ($h < 4) {
    $b += $c; $g += $x;
  } else if ($h < 5) {
    $b += $c; $r += $x;
  } else {
    $r += $c; $b += $x;
  }
  $r = min(255, max(0, round($r * 255)));
  $g = min(255, max(0, round($g * 255)));
  $b = min(255, max(0, round($b * 255)));
  return sprintf('#%02x%02x%02x', $r, $g, $b);
}

function ShowBattle($battle) {
  echo "<h3><a href=\"/vis/?battle_id={$battle['battle_id']}\">バトル {$battle['battle_id']}</a></h3>\n";
  echo "<table class=\"table table-striped table-bordered\" width=\"100%\">\n";
  echo '<tr><td>マップ</td><td>' . $battle['map_key'] . "</td></tr>\n";
  echo '<tr><td>作成時刻</td><td>' . $battle['battle_created'] . "</td></tr>\n";
  echo '<tr><td>更新時刻</td><td>' . $battle['battle_modified'] . "</td></tr>\n";
  if (isset($battle['battle_log_info']) && $battle['battle_log_info'] != '') {
    echo '<tr><td>エラー</td><td><pre style="margin:0">' . htmlspecialchars($battle['battle_log_info']) . '</pre></td></tr>';
  }
  echo '<tr><td>結果</td><td>';
  if (!isset($battle['punters'])) {
    echo 'Punter が存在しません';
  } else {
    echo '<table>';
    $ranks = [];
    foreach ($battle['punters'] as $punter) {
      @$ranks[$punter['punter_score']]++;
    }
    krsort($ranks);
    $rank = 1;
    foreach (array_keys($ranks) as $score) {
      $add = $ranks[$score];
      $ranks[$score] = $rank;
      $rank += $add;
    }

    $color_index = 0;
    foreach ($battle['punters'] as $punter) {
      $color = Color($color_index, count($battle['punters']));
      echo "<tr><td><span style=\"background:$color; color:#fff; display: inline-block; padding: 0 1ex; margin: 0.3ex 0;\">" . $color_index . '. ' . $punter['ai_key'] . '</span></td><td>&nbsp;…&nbsp;</td><td style="text-align:right"><span style="font-family:monospace">' . $punter['punter_score'] . '</span> 点</td><td style="text-align:right; padding: 0 1ex">( ' . $ranks[$punter['punter_score']] . ' 位 )</td></tr>';
      $color_index++;
    }
    echo '</table>';
    echo '</td></tr>';
  }
  echo "</table>\n";
}
