 import redis
 r = redis.StrictRedis(host='localhost',port=6379,db=0)
 r.set('foo', '123')
 True
 r.get('foo')
