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
 * 雪花算法生成ID工具类
 */
class SnowFlake
{
    /**
     * 配置参数
     * @var array
     */
    protected $config = [
        // 开始时间,固定一个小于当前时间的毫秒数即可
        'twepoch' => 1661155379000,
        // 数据中心ID
        'datacenter_id' => 0,
        // 数据中心ID占的位数
        'datacenter_id_bits' => 5,
        // 机器ID
        'worker_id' => 0,
        // 机器ID占的位数
        'worker_id_bits' => 5,
        // 毫秒内自增数点的位数
        'sequence_bits' => 12,
    ];

    /**
     * 时间毫秒要左移的位数
     * @var int
     */
    protected $timestampLeftShift = 0;

    /**
     * 数据中心ID要左移的位数
     * @var int
     */
    protected $dataCenterIdShift = 0;

    /**
     * 机器ID要左移的位数
     * @var int
     */
    protected $workerIdShift = 0;

    /**
     * 序列掩码
     * @var int
     */
    protected $sequenceMask = 0;

    /**
     * 上次时间戳
     * @var int
     */
    static $lastTimestamp = - 1;

    /**
     * 序列
     * @var int
     */
    static $sequence = 0;


    /**
     * 架构函数
     * @access public
     * @param array $options 配置参数
     */
    public function __construct($options = []){
        // 合并配置参数
        $this->config = array_merge($this->config, $options);
        //数据中心ID范围判断
        $maxDataCenterId = - 1 ^ (- 1 << $this->config['datacenter_id_bits']);
        if ($this->config['datacenter_id'] > $maxDataCenterId || $this->config['datacenter_id'] < 0) {
            throw new \Exception('数据中心ID必须在 0-' . $maxDataCenterId . ' 之间');
        }

        //机器ID范围判断
        $maxWorkerId = - 1 ^ (- 1 << $this->config['worker_id_bits']);
        if ($this->config['worker_id'] > $maxWorkerId || $this->config['worker_id'] < 0) {
            throw new \Exception('机器ID必须在 0-' . $maxWorkerId . ' 之间');
        }
        
        // 计算时间毫秒/数据中心ID/机器ID,要左移的位数
        $this->timestampLeftShift = $this->config['sequence_bits'] + $this->config['datacenter_id_bits'] + $this->config['worker_id_bits'];
        $this->dataCenterIdShift = $this->config['sequence_bits'] + $this->config['worker_id_bits'];
        $this->workerIdShift = $this->config['sequence_bits'];
        // 计算序列掩码
        $this->sequenceMask = - 1 ^ (- 1 << $this->config['sequence_bits']);
    }

    /**
     * 生成一个ID
     * @access public
     * @return int
     * @throws \Exception
     */
    public function generateId(){
        // 获取毫秒时间戳
        $timestamp = $this->timeGen();
        // 上次毫秒时间
        $lastTimestamp = self::$lastTimestamp;
        // 判断时钟是否正常
        if ($timestamp < $lastTimestamp) {
            throw new \Exception('时钟已回退，拒绝为 ' . ($lastTimestamp - $timestamp) . ' 毫秒之前的时间生成ID');
        }
        //生成唯一序列
        if ($lastTimestamp == $timestamp) {
            self::$sequence = (self::$sequence + 1) & $this->sequenceMask;
            if (self::$sequence == 0) {
                $timestamp = $this->tilNextMillis($lastTimestamp);
            }
        } else {
            self::$sequence = 0;
        }
        // 更新时间戳
        self::$lastTimestamp = $timestamp;
        // 组合4段数据返回: 时间戳.数据中心.工作机器.序列
        $nextId = (($timestamp - $this->config['twepoch']) << $this->timestampLeftShift) |
                    ($this->config['datacenter_id'] << $this->dataCenterIdShift) |
                    ($this->config['worker_id'] << $this->workerIdShift) |
                    self::$sequence;
        return $nextId;
    }

    /**
     * 取当前时间毫秒
     * @access public
     * @return float
     */
    protected function timeGen(){
        $timestramp = (float)sprintf('%.0f', microtime(true) * 1000);
        return $timestramp;
    }

    /**
     * 取下一毫秒
     * @access public
     * @param  string   $lastTimestamp  时间戳毫秒
     * @return float
     */
    protected function tilNextMillis($lastTimestamp){
        $timestamp = $this->timeGen();
        while ($timestamp <= $lastTimestamp) {
            $timestamp = $this->timeGen();
        }
        return $timestamp;
    }
}
