import httpx


async def get_client():
    # create a new client for each request
    async with httpx.AsyncClient(timeout=30) as client:
        # yield the client to the endpoint function
        yield client
        # close the client when the request is done
