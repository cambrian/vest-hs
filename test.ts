import { AddIntsRequest, ConcatTextAuthRequest, ConcatTextAuthResponse, Text } from './types'
import { call, makeDirectHandler, makeRequestOfAuth, makeStreamingHandler } from '../call'

import { BridgeClient } from '../client'
import { Stream } from '../streams'

export namespace Call {
  export async function addInts (
    bridgeClient: BridgeClient.T,
    request: AddIntsRequest
  ): Promise<number> {
    return call<AddIntsRequest, number>(
      bridgeClient,
      makeRequestOfAuth(),
      makeDirectHandler,
      'addInts',
      'number',
      request
    )
  }

  export async function echoThrice (
    bridgeClient: BridgeClient.T,
    request: number
  ): Promise<Stream<number>> {
    return call<number, Stream<number>>(
      bridgeClient,
      makeRequestOfAuth(),
      makeStreamingHandler,
      'echoThrice',
      'number',
      request
    )
  }

  export async function concatTextAuth (
    bridgeClient: BridgeClient.T,
    request: ConcatTextAuthRequest,
    token: Text<'AuthToken'>
  ): Promise<ConcatTextAuthResponse> {
    return call<ConcatTextAuthRequest, ConcatTextAuthResponse>(
      bridgeClient,
      makeRequestOfAuth(token),
      makeDirectHandler,
      'concatTextAuth',
      'ConcatTextAuthResponse',
      request
    )
  }

  export async function echoThriceAuth (
    bridgeClient: BridgeClient.T,
    request: string,
    token: Text<'AuthToken'>
  ): Promise<string> {
    return call<string, string>(
      bridgeClient,
      makeRequestOfAuth(token),
      makeDirectHandler,
      'echoThriceAuth',
      'string',
      request
    )
  }

}
