from pyrule.abstract.agenda import Agenda

# TODO
class ReduceAgenda(Agenda):
    """ Agenda to act like the functional programming concept. Take options and fold them """
    # ie [proposals] + [list_of_chars] -> [total_change_by_char]
    # could include saturation curves/limits

    def __init__(self, reduce_key, reduce_value, init_value, transform):
        super().__init__()
        self._reduce_key = reduce_key
        self._reduce_value = reduce_value
        self._init_value = init_value
        self._transform = transform

    def __call__(self, proposals, engine, **kwargs):
        reductions = {}

        for data, actions in proposals:
            if data[self._reduce_key] not in reductions:
                base_data = {}.update(data)
                base_data[data[self._reduce_value]] = self._init_value
                reductions[data[self._reduce_key]] = (base_data, actions)

            curr_val = reductions[data[self._reduce_key]][self._reduce_value]
            updated_value = self._transform(curr_val, data[self._reduce_value])
            reductions[data[self._reduce_key]][self._reduce_key] = updated_value

        return reductions.values()
