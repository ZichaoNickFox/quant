module Web.View.NoteView where
import IHP.ViewPrelude
import Web.Types

data NoteView = NoteView
  { noteId :: Id Note
  , noteCells :: [Cell]
  }

instance View NoteView where
  html NoteView { noteId, noteCells } = [hsx|
    <div class="row">
      <div class="col-3 border-end">
        <h5>笔记树</h5>
        <div
          data-tree-root="1"
          data-owner-type="note"
          data-owner-id={tshow noteId}
          style="min-height: 220px;"
        ></div>
      </div>
      <div class="col-9">
        <h3 data-cell-title="1">笔记详情</h3>
        <div data-cell-create="1" data-owner-type="note">
          <input type="hidden" name="ownerType" value="note" />
          <input type="hidden" name="ownerId" value={noteId} data-cell-create-owner-id="1" />
        </div>
        <div data-cell-list="1">
          {forEach noteCells renderCell}
        </div>
      </div>
    </div>
  |]
    where
      renderCell cell = [hsx|
        <div class="card mb-2">
          <div class="card-body">
            <div data-cell-update-card="1" data-owner-type="note">
              <input type="hidden" name="cellId" value={get #id cell} />
              <div class="d-flex gap-2 flex-wrap align-items-center mb-2">
                <select name="cellType" class="form-select form-select-sm w-auto" style="height: 32px;">
                  {forEach [Raw, Image, Backtest, Chart] (cellTypeOption (get #cellType cell))}
                </select>
                <button class="btn btn-sm btn-outline-secondary" type="button" data-cell-save-button="1" style="height: 32px;">Save</button>
                <button class="btn btn-sm btn-outline-danger" type="button" data-cell-delete-button="1" style="height: 32px;">Delete</button>
                <button class="btn btn-sm btn-outline-secondary" type="button" data-cell-add-above="1" style="height: 32px;">Insert Above</button>
                <button class="btn btn-sm btn-outline-secondary" type="button" data-cell-add-below="1" style="height: 32px;">Insert Below</button>
                <button class="btn btn-sm btn-outline-secondary" type="button" data-cell-move-up="1" style="height: 32px;">Move Up</button>
                <button class="btn btn-sm btn-outline-secondary" type="button" data-cell-move-down="1" style="height: 32px;">Move Down</button>
              </div>
              <textarea class="form-control" name="content" rows="4">{fromMaybe "" (get #content cell)}</textarea>
            </div>
          </div>
        </div>
      |]

      cellTypeOption current cellType = [hsx|
        <option value={inputValue cellType} selected={cellType == current}>{show cellType}</option>
      |]
