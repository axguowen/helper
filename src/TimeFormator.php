<?php
// +----------------------------------------------------------------------
// | Helper [PHP Helpler Library]
// +----------------------------------------------------------------------
// | Helper
// +----------------------------------------------------------------------
// | Licensed ( http://www.apache.org/licenses/LICENSE-2.0 )
// +----------------------------------------------------------------------
// | Author: axguowen <axguowen@qq.com>
// +----------------------------------------------------------------------

namespace axguowen\helper;

/**
 * 时间格式化工具
 */
class TimeFormator
{
	/**
	 * 返回格林威治标准时间
	 * @param string $format 字符串格式
	 * @param string $mktime 时间戳
	 * @param string $timezone 时区
	 * @return string
	 */
    public static function myDate($format='Y-m-d H:i:s', $mktime=null, $timezone=8)
    {
		// 未指定则默认为当前时间
		if(is_null($mktime)) $mktime = time();
        return gmdate($format, $mktime + $timezone * 3600);
    }
	
	/**
	 * 从普通时间转换为Linux时间截
	 * @param string   $dtime  普通时间
	 * @return string
	 */
	public static function getMkTime($dtime)
	{
		// 默认时间数组
		$dt = Array(1970, 1, 1, 0, 0, 0);
		// 非时间戳
		if(!preg_match('/[^0-9]/', $dtime)){
			return $dtime;
		}
		$dtime = trim($dtime);
		$dtime = preg_replace("/[\r\n\t]|日|秒/", ' ', $dtime);
		$dtime = str_replace('年', '-', $dtime);
		$dtime = str_replace('月', '-', $dtime);
		$dtime = str_replace('时', ':', $dtime);
		$dtime = str_replace('分', ':', $dtime);
		$dtime = trim(preg_replace('/[ ]{1,}/', ' ', $dtime));
		$ds = explode(' ', $dtime);
		// 日期处理
		$ymd = explode('-', $ds[0]);
		if(!isset($ymd[1])){
			$ymd = explode('.', $ds[0]);
		}
		if(isset($ymd[0])) $dt[0] = $ymd[0];
		if(isset($ymd[1])) $dt[1] = $ymd[1];
		if(isset($ymd[2])) $dt[2] = $ymd[2];
		// 补齐年份
		if(strlen($dt[0]) == 2) $dt[0] = '20' . $dt[0];

		// 时间处理
		if(isset($ds[1])){
			$hms = explode(':', $ds[1]);
			if(isset($hms[0])) $dt[3] = $hms[0];
			if(isset($hms[1])) $dt[4] = $hms[1];
			if(isset($hms[2])) $dt[5] = $hms[2];
		}

		foreach($dt as $k => $v)
		{
			$v = preg_replace('/^0{1,}/', '', trim($v));
			if($v == ''){
				$dt[$k] = 0;
			}
		}
		$mt = mktime($dt[3], $dt[4], $dt[5], $dt[1], $dt[2], $dt[0]);
		if(!empty($mt)){
			return $mt;
		}
		else{
			return time();
		}
    }
	
	/**
	 *  增减天数
	 *
	 * @param     int  $days   	增减天数
	 * @param     int  $mktime	指定时间戳
	 * @return    int
	 */
	public static function incDays($days, $mktime=null)
	{
		// 默认为当前时间
		if(is_null($mktime)) $mktime = time();
		return strtotime($days . ' day', $mktime);
	}

	/**
	 *  增减月数
	 *
	 * @param     int  $months   	增减月数
	 * @param     int  $mktime		指定时间戳
	 * @return    int
	 */
	public static function incMonths($months, $mktime=null)
	{
		// 默认为当前时间
		if(is_null($mktime)) $mktime = time();
		return strtotime($months . ' month',$mktime);
	}

	/**
	 *  增减年数
	 *
	 * @param     int  $years   	增减年数
	 * @param     int  $mktime		指定时间戳
	 * @return    int
	 */
	public static function incYears($years, $mktime)
	{
		// 默认为当前时间
		if(is_null($mktime)) $mktime = time();
		return strtotime($years . ' year',$mktime);
	}

	/**
	 *  获取指定月份的第一天日期
	 *
	 * @param     int  $mktime		指定时间戳
	 * @return    string
	 */
	public static function getMonthFirstDate($mktime=null)
	{
		// 默认为当前时间
		if(is_null($mktime)) $mktime = time();
		return self::myDate('Y-m-01', $mktime);
	}

	/**
	 *  获取指定月份的最后一天日期
	 *
	 * @param     int  $mktime		指定时间戳
	 * @return    string
	 */
	public static function getMonthLastDate($mktime=null)
	{
		// 默认为当前时间
		if(is_null($mktime)) $mktime = time();
		// 获取当前月第一天时间戳
		$month_start = self::getMkTime(self::myDate('Y-m-01', $mktime));
		// 获取最后一天
		$month_end = strtotime('+1 month -1 seconds', $month_start);
		// 返回
		return self::myDate('Y-m-d', $month_end);
	}

	/**
	 *  获取指定日期开始时间
	 *
	 * @param     int    $mktime  时间戳
	 * @return    string
	 */
	public static function getDateStartTime($mktime=null)
	{
		// 默认为当前时间
		if(is_null($mktime)) $mktime = time();
		return self::getMkTime(self::myDate('Y-m-d 00:00:00', $mktime));
	}

	/**
	 *  获取指定日期结束时间
	 *
	 * @param     int    $mktime  时间戳
	 * @return    string
	 */
	public static function getDateEndTime($date)
	{
		// 默认为当前时间
		if(is_null($mktime)) $mktime = time();
		return self::getMkTime(self::myDate('Y-m-d 23:59:59', $mktime));
	}
	
	/**
	 *  返回格式化(Y-m-d H:i:s)的是时间
	 *
	 * @param     int    $mktime  时间戳
	 * @return    string
	 */
	public static function getDateTimeMk($mktime=null)
    {
		// 默认为当前时间
		if(is_null($mktime)) $mktime = time();
        return self::myDate('Y-m-d H:i:s',$mktime);
    }
	
	/**
	 *  返回格式化(Y-m-d)的日期
	 *
	 * @param     int    $mktime  时间戳
	 * @return    string
	 */
	public static function getDateMk($mktime=null)
    {
		// 默认为当前时间
		if(is_null($mktime)) $mktime = time();
        return self::myDate('Y-m-d', $mktime);
    }
	
	/**
	 *  返回格式化(Y-m)的月份
	 *
	 * @param     int    $mktime  时间戳
	 * @return    string
	 */
	public static function getMonthMk($mktime=null)
    {
		// 默认为当前时间
		if(is_null($mktime)) $mktime = time();
        return self::myDate('Y-m', $mktime);
    }
	
	/**
	 *  返回格式化(Y)的年份
	 *
	 * @param     int    $mktime  时间戳
	 * @return    string
	 */
	public static function getYearMk($mktime=null)
    {
		// 默认为当前时间
		if(is_null($mktime)) $mktime = time();
        return self::myDate('Y', $mktime);
    }

	/**
	 *  获取时间差值转化为XX分XX秒
	 *
	 * @param     int   $endTime  	指定时间戳
	 * @param     int   $startTime  开始时间戳
	 * @return    string
	 */
	public static function getDateCompared($endTime, $startTime=null)
	{
		// 默认为当前时间
		if(is_null($startTime)) $startTime = self::myDate();
		$datetime1 = new \DateTime($startTime);
		$datetime2 = new \DateTime($endTime);
		$interval = $datetime2->diff($datetime1);
		$dateStr = '';
		// 年月日时分秒
		$years = intval($interval->format('%Y'));
		if($years > 0){
			$dateStr .= $years . '年';
		}
		$months = intval($interval->format('%m'));
		if(!empty($dateStr) || $months > 0){
			$dateStr .= $months . '个月';
		}
		$days = intval($interval->format('%d'));
		if(!empty($dateStr) || $days > 0){
			$dateStr .= $days . '天';
		}
		$hours = intval($interval->format('%H'));
		if(!empty($dateStr) || $hours > 0){
			$dateStr .= $hours . '小时';
		}
		$minutes = intval($interval->format('%i'));
		if(!empty($dateStr) || $minutes > 0){
			$dateStr .= $minutes . '分钟';
		}
		$seconds = intval($interval->format('%s'));
		if(!empty($dateStr) || $seconds > 0){
			$dateStr .= $seconds . '秒';
		}
		// 两个时间相差总天数
		$totalDays = intval($interval->format('%a'));
		// 返回
		return $dateStr;
	}

	/**
     * 获取指定时间距离现在多少天
     * @param 	string 		$endDate 指定日期：2022-07-21
     * @return 	string
     */
    public static function getTimeComparedNow($endDate)
    {
		// 默认为当前时间
		$startTime = time();
		// 时间差
        $diff = $startTime - self::getMkTime($endDate);
		// 获取本月时间差值
		$monthTimes = $startTime - strtotime('-1 month', $startTime);
		// 获取本年时间差值
		$yearTimes = $startTime - strtotime('-1 year', $startTime);
        if ($diff <= 60) {
            return $diff . '秒前';
        } elseif ($diff <= 3600) {
            return floor($diff / 60) . '分钟前';
        } elseif ($diff <= 86400) {
            return floor($diff / 3600) . '小时前';
        } elseif ($diff <= $monthTimes){
            return floor($diff / 86400) . '天前';
        } elseif ($diff <= $yearTimes){
            return floor($diff / (86400 * 30)) . '个月前';
        } else {
            return '一年前';
        }
    }
}
