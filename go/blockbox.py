"""
A simple web server, providing a restful API for 1KB data blocks.
"""
import tornado.web
import tornado.wsgi
import tornado.httpserver
import tornado.ioloop
import re

rexVolume = '[a-zA-Z0-9-]'

## the block device ############################################

class BlockDevice(object):
    """
    This simulates a small block-based storage device, with direct access
    to 65536 1-kilobyte data blocks. The files are stored in *.dat files,
    in a directory corresponding to the volume label.
    """
    kSize = 1024
    root  = 'vol' # ex: ./vol/name/89AB.dat

    def __init__(self, volume):
        assert re.match(rexVolume, volume), 'invalid volume name'
        self.volume = volume

    def _path(self, n):
        """
        the blocks are sequenced in hexidecimal-numbered *.dat files.
        """
        return os.path.join(self.root, self.volume, '%04X.dat' % n)

    def _open(self, n, mode):
        return open(self._path(n), mode)

    ## public interface ########################################

    def fetch(self, n):
        path = self._path(n)
        if os.path.exists(path):
            return open(path, 'r').read(kSize)
        else: return None

    def store(self, n, data):
        with _open(n, 'wb') as block:
            # trim if too big, pad if too small:
            block.write(bytes(data[:kSize]))
            block.write(bytes('\0' * (kSize-block.tell())))

    def blank(self):
        return bytes('\0' * self.kSize)



## wsgi interface ##############################################

kOctet = 'application/octet-stream'

def _head(mimetype):
    return [('Content-type', kOctet)]

def error(env, start, status=404, msg='Not Found'):
    start('%i %s' % (status, msg), head('text/html'))
    return ('<html><title>%i %s</title> <h1>%i %s</h1></html>'
            % (status, msg, status, msg))

def homepage(env, start):
    start('200 OK' , header('text/html'))
    return open('blockbox.html').read()

def get_block(env, start):
    start('200 OK', header(kOctet))
    return device.get(blockn)

def put_block(env, start):
    start('202 OK', header(kOctet))
    return device.get(blockn)

def del_block(env, start):
    pass


dispatch = {
    'GET' : get_block,
    'PUT' : put_block,
    'DELETE': del_block }
    
def blockapi(env, start):
    volume, hexstr = env['PATH_INFO'].split('/')
    blockn = int(hexstr, 16)
    device = BlockDevice(volume)
    try: return dispatch[env['HTTP_METHOD']](env, start)
    except KeyError:
        return error(env, start, 405, 'Not Allowed')


## web server ##################################################

def main(host, port):

    app = tornado.web.Application([
        (r'/$', tornado.wsgi.WSGIContainer(homepage)),
        # (r'/v/?$', tornado.wsgi.WSGIContainer(volumes)),
        # (r'/v/%s/?$' % rexVolume, tornado.wsgi.WSGIContainer(volume)),
        (r'/v/%s/[a-f0-9]{4}$' % rexVolume, tornado.wsgi.WSGIContainer(blockapi)),
        (r'.*', tornado.web.FallbackHandler, {
            'fallback': tornado.wsgi.WSGIContainer(error) }),
    ], debug=False)

    server = tornado.httpserver.HTTPServer(app)
    server.listen(address=host, port=port)

    print("starting server at http://{0}:{1}/".format(host, port))
    tornado.ioloop.IOLoop.instance().start()

if __name__=="__main__":
    main(host='0.0.0.0', port='9876')
